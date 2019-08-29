

##############################################################################

### Functions for Bayesian Network Analysis of USA agricultural land use change

### Functions together learn structures and parameters of subcomponent Bayesian Network models.

###############################################################################


library(bnlearn)        ### machine learning of bayesian networks
library(foreach)        ### parralel processing
library(doParallel)     ### parallel processing
library(parallel)       ### parallel processing
library(plyr)           ### data management
library(dplyr)          ### data management
library(Metrics)        ### used to calculate RMSE
library(rstanarm)       ### Bayesian GLMs for node parameters


#################################################################################################

### 1) Time break identification & Markovian Blanket Feature selection

#################################################################################################


Dynamic.MB <- function(data, niter, var.cols, method = c('Constraint', 'Score'), target, 
                       koralpha = 'k', k = 3, alpha = 0.5) {
  
  
  ####################
  ### Learns a Markovian Blanket of a target variable from the data in k temporal segments
  ####################
  
  
  require(dplyr)
  require(bnlearn)
  require(EDISON)
  require(foreach)
  
  data.res <- list()
  
  data.res$Breaks <- list()
  data.res$MB     <- list()
  
  cl <- makeCluster(3)
  registerDoParallel(cl)
  
  
  set.seed(101)
  
  #####################################################################
  
  ### Create temporal break points
  
  #####################################################################
  
  CPP      <- foreach(m=1:100) %dopar% { 
    
    require(dplyr)
    require(bnlearn)
    require(EDISON)
    require(foreach)
    
    dat.sample <- sample(nrow(data), nrow(data), replace = TRUE)
    
    dat     <- data[dat.sample, ] %>% group_by(Year) %>% summarise_at(4:ncol(data), mean, na.rm = TRUE)
    
    m       <- as.matrix(dat[, -c(1)])
    tgt     <- as.matrix(dat[, 2:ncol(dat)])
    
    obs     <- nrow(tgt)
    
    ### Come back to this?
    
    EDISON1 <- runDBN(t(tgt), t(m), q = ncol(tgt), n = obs, multipleVar = TRUE,
                      minPhase = 2, niter = 10000, scaling = FALSE, method = "poisson",
                      prior.params = NULL, self.loops = TRUE, k = 15, options = NULL,
                      outputFile = "E:/Modules/Dissertation/Data/Model Ready/EDISON", fixed.edges = NULL)
    
    
    
    CPP     <- calculateCPProbabilities(EDISON1)$global.cps
    
  }
  
  CPP.df <- rbind.fill(lapply(CPP, data.frame))
  
  CPP    <- colMeans(CPP.df)
  
  #CPP     <- which(CPP > 0.66) - 1
  
  if(koralpha == 'k') {
    
    top    <- length(CPP)
    bottom <- top - 2
    
    
    CPP    <- order(CPP)[bottom:top]
    CPP    <- CPP - 1
    
  } else if(koralpha != 'k') {
    
    CPP     <- which(CPP >= alpha) - 1
    
  }
  
  
    ########################
    
    ### Convert study year integer (1-28) to calendar year (1990-2017)
    
    #########################
    
  
  for(n in 1:length(CPP)) {
    
    CPP[n]     <- EDISON.convert$ï..Year[which(CPP[n] == EDISON.convert$CPP)]
    
  }
  
  CPP     <- CPP[!CPP %in% c(1990)]
  
  data.res$Breaks <- CPP
  
  dat.list<- list()
  
  for(n in 1:length(CPP)) {
    
    if(!is.na(CPP[n+1])) {
      
      if(length(CPP[n-1]) != 0) {
        
        dat.list[[n]] <- data %>% filter(Year < CPP[[n]] & Year >= CPP[[n-1]])
        
      } else if(length(CPP[n-1]) == 0) {
        
        
        dat.list[[n]] <- data %>% filter(Year < CPP[[n]])
        
      }
      
      
      
    } else if(is.na(CPP[n+1])) {
      
      dat.list[[n]] <- data %>% filter(Year <= CPP[[n]])
      
    }
    
    
  }
  
  
  ### prepare markov blanket
  print(paste0('Learning Markov Blankets'))
  data.res$MB <- list()
  
  ####################################
  ### Run Markovian Blanket feature selection
  ####################################
  
  for(j in 1:length(dat.list)) {
    
    cl <- makeCluster(3)
    registerDoParallel(cl)
    
    
    ### bagging 
    
    if(method == 'Constraint') {
      
      ### A) Grow shrink
      
      data.mb      <- foreach(m=1:10000) %dopar% { 
        
        # using sample function without seed
        
        sampleRows <- base::sample(nrow(dat.list[[j]]), size=floor((nrow(dat.list[[j]]))), replace = TRUE)
        
        data.mb    <- bnlearn::learn.mb(x = dat.list[[j]][sampleRows, var.cols], node = target, method = 'gs')
        
      } 
      
      
      data.res$MB[[j]]    <- table(unlist(data.mb))
      
    } else if(method == 'Score') {
      
      ### A) Score-based
      
      data.mb      <- foreach(m=1:10000) %dopar% { 
        
        # using sample function without seed
        
        sampleRows <- base::sample(nrow(dat.list[[j]]), size=floor((nrow(dat.list[[j]]))), replace = TRUE)
        
        data.bn    <- bnlearn::hc(x = dat.list[[j]][sampleRows, var.cols])
        
        data.mb    <- bnlearn::mb(data.bn, node = target)
        
      } 
      
      
      data.res$MB[[j]]    <- table(unlist(data.mb))
      
    }
    
    print(j)
    
  }
  
  
  return(data.res)
  
}


##############################################################################################

### 2) Learn Network structures

##############################################################################################



Learn.Arcs   <- function(data, MBList, MB.threshold = 0.85, alpha = 0.95, target.node = 'Yield', 
                         noarc = NULL, R = 250) {
  
  
  
  #######
  ## Learns Bayesian Network Structure given a target variable, feature screening and temporal subcomponents
  ## Temporal sub components & feature selection are done using MB.List, the output of Dynamic.MB function
  #######

  
  bn.list      <- list()
  bn.results   <- list()
  data.filter  <- list()  
  
  for(i in 1:6) {
    
    
    ###############
    
    ### Use breaks from Dynamic.MB list output
    
    ###############
    
    for(j in 1:length(MBList[[i]]$Breaks)) {
      
      if(MBList[[i]]$Breaks[j] != 2017)  {
        
        data.filter[[j]] <- data[[i]][data[[i]]$Year < MBList[[i]]$Breaks[j], ]
        
      } else if (MBList[[i]]$Breaks[j] == 2017) {
        
        data.filter[[j]] <- data[[i]][data[[i]]$Year >= MBList[[i]]$Breaks[j-1], ]
        
      }
      
    }
    
    
    
    #################################
    
    ### filter for is in MB numbs
    
    #################################
    
    
    for(j in 1:length(MBList[[i]]$MB)) {
      
      MB                      <- MBList[[i]]$MB[[j]]
      MB                      <- MB[(MB / max(MB)) > MB.threshold]
      
      
      col.key                 <- colnames(data.filter[[j]]) %in% c(names(MB), target.node, 'PreGM_Acres')
      
      data.filter[[j]]        <- data.frame(data.filter[[j]])[, col.key]
      
    }
    

    
    
    
    ###################
    
    ### 2) set up BN models
    
    ###################
    
    
    for(j in 1:length(data.filter)) {
      
      temp.noarc       <- noarc[[i]]
      
      print('Learning model')
      
      filt.cols        <- colnames(data.filter[[j]])
      
      temp.noarc       <- data.frame(temp.noarc %>% filter(from %in% filt.cols & to %in% filt.cols))
      
      if(nrow(temp.noarc) == 0) {
        
        bn.list[[j]] <- ''
        
        next()
        
      }
      
      cl <- makeCluster(3)
      registerDoParallel(cl)
      
      boot.bn      <- boot.strength(data.filter[[j]], R = R, algorithm = 'hc', 
                                    algorithm.args = list(blacklist = temp.noarc), cluster = cl)
      
      ###################################
      
      ### Extract averaged network
      
      ###################################
      
      boot.bn      <- boot.bn[boot.bn$direction >= 0.8, ]
      
      if(length(boot.bn >= 1)) {
        
        boot.filt    <- averaged.network(boot.bn, threshold = alpha)
        
        boot.filt    <- boot.bn[boot.bn$from %in% c(mb(boot.filt, target.node), target.node, 'Pre_GM.pct') & 
                                  boot.bn$to %in% c(mb(boot.filt, target.node), target.node, 'Pre_GM.pct'), ]
        
        if(nrow(boot.filt) > 1) {
          
          boot.filt    <- averaged.network(boot.filt, threshold = alpha)
          
        } else {
          
          boot.filt <- ''
          
        }
        
      } else {
        
        boot.filt <- ''
        
      }
      
      bn.list[[j]] <- boot.filt
      
    }
    
    bn.results[[i]]   <- c(bn.list)
    
    
    print(paste0(i, ' of ', length(data), ' complete.'))
    
  }
  
  return(bn.results)
  
}




###############################################################################

### 3) MLE Parameter Learning

###############################################################################



learn.Parameters <- function(model, breaks, data.test, data.train, target =  'Yield') {

model.parameters <- list()

model.strength   <- list()

BIC              <- list()

for(i in 1:6) {

  model.parameters[[i]] <- list()
  
  model.strength[[i]]   <- list() 
  
  BIC[[i]]              <- list()
  
    for(j in 1:length(Corn.90[[i]])) {

      
      temp.dat                   <- data.frame(data.train[[i]])
      
      
      if(breaks[[i]]$Breaks[j] != 2017) {
        
        temp.dat                   <- data.frame(temp.dat %>% filter(Year < breaks[[i]]$Breaks[j]))
        
      } else if(breaks[[i]]$Breaks[j] == 2017) {
        
        
        temp.dat                   <- data.frame(temp.dat %>% filter(Year >= breaks[[i]]$Breaks[j-1]))
        
        
      }

      try(temp.dat                   <- data.frame(temp.dat[, c(colnames(temp.dat) %in% 
                                                                   nodes(model[[i]][[j]]))]))
      
      #temp.dat$DELTA_Corn        <- unlist(temp.med)
  
  
  try(model.parameters[[i]][[j]]      <- bn.fit(model[[i]][[j]], temp.dat))
 
  #model.strength[[i]][[j]]       <- summary(lm(Yield ~., data = temp.dat))

  try(BIC[[i]][[j]]                  <- score(model[[i]][[j]], temp.dat))
  
  }

}






#######################################################################################################
### Use model to predict
#######################################################################################################

bn.res <- list()

for(i in 1:length(model)) {
  
  bn.res[[i]] <- list()
  
  for(j in 1:2) {
  
      temp.dat                   <- data.frame(data.test[[i]])
    
    
    if(breaks[[i]]$Breaks[j] != 2017) {
      
      temp.dat                   <- data.frame(temp.dat %>% filter(Year < breaks[[i]]$Breaks[j]))
      
    } else if(breaks[[i]]$Breaks[j] == 2017) {
      
      
      temp.dat                   <- data.frame(temp.dat %>% filter(Year >= breaks[[i]]$Breaks[j-1]))
      
      
    }
    
    temp.dat                   <- data.frame(temp.dat[, c(colnames(temp.dat) %in% 
                                                            nodes(model[[i]][[j]]))])
    
    
      
    try(res <- predict(model.parameters[[i]][[j]], target, temp.dat, method = 'bayes-lw'))
    
    try(bn.res[[i]][[j]] <- res)
    
  }
  

  
}



for(i in 1:length(bn.res)) {
  
  for(j in 1:2) {
    
    temp.dat                   <- data.frame(data.test[[i]])
    
    
    if(breaks[[i]]$Breaks[j] != 2017) {
      
      temp.dat                   <- data.frame(temp.dat %>% filter(Year < breaks[[i]]$Breaks[j]))
      
    } else if(breaks[[i]]$Breaks[j] == 2017) {
      
      
      temp.dat                   <- data.frame(temp.dat %>% filter(Year >= breaks[[i]]$Breaks[j-1]))
      
      
    }

    try(print(plot(bn.res[[i]][[j]], temp.dat$Yield)))
    
    try(abline(coef = c(0, 1)))
    
    try(bn.res[[i]][[j]]           <- rmse(temp.dat$Yield, bn.res[[i]][[j]]))
    
  }
  
}

return(list(model.parameters, bn.res, BIC))

}



###########################################################################################################################

### 4) Bayesian Parameter Learning

###########################################################################################################################


### lambda function from stackoverflow, creates formula objects on the fly from dataframe column names

### https://stackoverflow.com/questions/25954361/how-to-insert-dataframe-column-name-into-equation-r

xyform <- function (y_var, x_vars) {
  # y_var: a length-one character vector
  # x_vars: a character vector of object names
  as.formula(sprintf("%s ~ %s", y_var, paste(x_vars, collapse = " + ")))
}

options(mc.cores = parallel::detectCores())


########################################################################

### Yield

########################################################################

### Code is for Corn parameters

stanmods.Corn <- list()

for(i in 1:length(FINAL.Corn)) {
  
  stanmods.Corn[[i]] <- list()
  
  for(j in 1:length(FINAL.Corn[[i]])) {
    
    if(CornMedian.score[[i]][j] != 2017) {
      
      dat.temp <- Corn.median[[i]][Corn.median[[i]]$Year < CornMedian.score[[i]][j], ]
      
    } else if(CornMedian.score[[i]][j] == 2017) {
      
      dat.temp <- Corn.median[[i]][Corn.median[[i]]$Year >= CornMedian.score[[i]][j-1], ]
      
    }
    
    dat.temp <- dat.temp[, which(colnames(dat.temp) %in% c(FINAL.Corn[[i]][[j]]$Yield$node, 
                                                           FINAL.Corn[[i]][[j]]$Yield$parents))]
    
    stanmods.Corn[[i]][[j]]  <- stan_glm(Yield ~. - Yield, 
                                    algorithm = 'sampling', data = dat.temp, iter = 2000, chains = 4) 
    
  }
  
}



#####################################################################################

### Yield Price Dependencies

#####################################################################################

stanmods.Price.Corn <- list()

for(i in 1:length(FINAL.Corn)) {
  
  stanmods.Price.Corn[[i]] <- list()
  
  for(j in 1:length(FINAL.Corn[[i]])) {
    
    stanmods.Price.Corn[[i]][[j]] <- list()
    
    if(CornMedian.score[[i]][j] != 2017) {
      
      dat.temp <- Corn.median[[i]][Corn.median[[i]]$Year < CornMedian.score[[i]][j], ]
      
    } else if(CornMedian.score[[i]][j] == 2017) {
      
      dat.temp <- Corn.median[[i]][Corn.median[[i]]$Year >= CornMedian.score[[i]][j-1], ]
      
    }
    
    for(k in 1:length(FINAL.Corn[[i]][[j]])) {
      
      kids <- FINAL.Corn[[i]][[j]][[k]]$parents
      
      print(kids)
      
      
        if('Expection' %in% kids | 'Price_lag1' %in% kids) {
          
          dat.temp2 <- dat.temp[, which(colnames(dat.temp) %in% c(FINAL.Corn[[i]][[j]][[k]]$node, 
                                                                  FINAL.Corn[[i]][[j]][[k]]$parents))]
          
          
          form <- xyform(y_var = FINAL.Corn[[i]][[j]][[k]]$node, x_vars = colnames(dat.temp2)[
            colnames(dat.temp2) != FINAL.Corn[[i]][[j]][[k]]$node])
          
          stanmods.Price.Corn[[i]][[j]][[k]]  <- stan_glm(form, 
                                                    algorithm = 'sampling', data = dat.temp2, iter = 2000, chains = 4) 
                                                     
          
          
        }
        
        
      } 
    
    }
      
  } 
    


#############################################################################################

### Extensification

#############################################################################################


stanmods.Extense.Corn <- list()

for(i in 1:length(FINAL.CornExtense)) {
  
  stanmods.Extense.Corn[[i]] <- list()
  
  for(j in 1:length(FINAL.CornExtense[[i]])) {
    
    stanmods.Extense.Corn[[i]][[j]] <- list()
    
    if(DELTA_Corn.res[[i]][j] != 2017) {
      
      dat.temp <- Corn.Extense[[i]][Corn.Extense[[i]]$Year < DELTA_Corn.res[[i]][j], ]
      
    } else if(DELTA_Corn.res[[i]][j] == 2017) {
      
      dat.temp <- Corn.Extense[[i]][Corn.Extense[[i]]$Year >= DELTA_Corn.res[[i]][j-1], ]
      
    }
    
    dat.temp <- dat.temp[, which(colnames(dat.temp) %in% c(FINAL.CornExtense[[i]][[j]]$DELTA_Corn$node, 
                                                           FINAL.CornExtense[[i]][[j]]$DELTA_Corn$parents))]
    
    if(! 'PreGM_Acres' %in% FINAL.CornExtense[[i]][[j]]$DELTA_Corn$parents) {
      
      
      stanmods.Extense.Corn[[i]][[j]]  <- stan_glm(DELTA_Corn ~. - DELTA_Corn, 
                                                  algorithm = 'sampling', data = dat.temp, iter = 2000, chains = 4) 
      
    } else if('PreGM_Acres' %in% FINAL.CornExtense[[i]][[j]]$DELTA_Corn$parents) {
      
      
      for(level in 1:3) {
        
        
        dat.temp2 <- dat.temp[as.numeric(dat.temp$PreGM_Acres) == level, ]
        
        
        if(nrow(dat.temp2) > 0) {
          
          stanmods.Extense.Corn[[i]][[j]][[level]]  <- stan_glm(DELTA_Corn ~. - DELTA_Corn, 
                                                               algorithm = 'sampling', data = dat.temp2[, colnames(dat.temp2) != 'PreGM_Acres'], iter = 2000, chains = 4)
          
        }
        
      }
      
    }
    
    
    
  }
  
}


