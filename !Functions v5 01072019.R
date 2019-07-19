

##############################################################################

### Functions for Bayesian Network Analysis of USA agricultural land use change

### Author: Oli Perkins May-July 2019

###############################################################################


library(bnlearn)
library(foreach)
library(doParallel)
library(plyr)
library(dplyr)
library(Metrics)



### 1) Piece-wise Markovian Blanket Feature selection

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




###################################################################################

### 3) Run the model

###################################################################################



#***********************************************************************************************

################################################################################################

### Model function

################################################################################################

#***********************************************************************************************

SoyProduction.Results  <- list()
SoyIntense.Results     <- list()
SoyExtense.Results     <- list()
SoyDelta.Results       <- list()
SoyMargin.Results      <- list()
SoyPrice.Results       <- list()


CornProduction.Results <- list()
CornIntense.Results    <- list()
CornExtense.Results    <- list()
CornDelta.Results      <- list()
CornMargin.Results     <- list()
CornPrice.Results      <- list()


for(year in 1990:2017) {

  
  temp.Soyproduction  <- list()
  temp.Soyyield       <- list() 
  temp.Soyextense     <- list()
  temp.SoyDelta       <- list()
  temp.SoyDifference  <- list()
  
  temp.Cornproduction  <- list()
  temp.Cornyield       <- list() 
  temp.Cornextense     <- list()
  temp.CornDelta       <- list()
  temp.CornDifference  <- list()

  
  for(i in 1:length(Soyintense.dat)) {

  
  ##############################################################################
  
  ### Soy intensification
  
  ##############################################################################
  
  Soy.break                 <- ifelse(year < Soyintense.breaks[[i]][1], 1, 2)
  
  Soyintense.dat.temp       <- data.frame(Soyintense.dat[[i]] %>% 
                                 filter(Year == year))[, 4:ncol(Soyintense.dat[[i]])]
  
  Soyintense.dat.temp       <- Soyintense.dat.temp[, colnames(Soyintense.dat.temp) %in% nodes(Soy.parameters[[i]][[Soy.break]])]
  
  
  ############
  ### Predict Intensification Factors from Price
  ############
  
  
  temp.arc                  <- arcs(Soy.parameters[[i]][[Soy.break]])
  temp.arc                  <- temp.arc[temp.arc[, 1] %in% c('Price_lag1', 'Expection')]
  temp.arc                  <- temp.arc[temp.arc != 'Expection' & temp.arc != 'Price_lag1' & temp.arc != 'Machinery_Dollar']
  temp.arc                  <- temp.arc[temp.arc != 'Acres_Planted']
  temp.arc                  <- unique(temp.arc)
  
  if(length(temp.arc)>0) {
    
    price.update <- list()
    
    for(j in 1:length(temp.arc)) {
    
    price.update[[j]]                    <- predict(Soy.parameters[[i]][[Soy.break]], node = temp.arc[j], 
                                                        data = Soyintense.dat.temp, 
                                                             method = 'bayes-lw')
    
    ### Constrain irrigation
    
      if(temp.arc[j] == 'irrigatedpct') {
      
      price.update[[j]] <- ifelse(Soyintense.dat.temp$irrigatedpct == 0, 0, unlist(price.update[[j]]))

      }
    
    }
      
    for(j in 1:length(price.update)) {
      
      Soyintense.dat.temp[, 
            which(colnames(Soyintense.dat.temp) == 
                                  temp.arc[j])]      <- price.update[[j]]
      
    }
    
  }
  
  
  #######################
  ### Constraints on values
  #######################
  
  Soyintense.dat.temp[, colnames(Soyintense.dat.temp) != 'Expection']       <- data.frame(apply(Soyintense.dat.temp[, colnames(Soyintense.dat.temp) != 'Expection'] , 2, function(x) {ifelse(x < 0, 0, x)}))
  
  
  if('GM' %in% colnames(Soyintense.dat.temp)) {
  
  Soyintense.dat.temp$GM    <- ifelse(year < 1997, 0, Soyintense.dat.temp$GM)
  
  Soyintense.dat.temp$GM   <- ifelse(Soyintense.dat.temp$GM > 100, 100, Soyintense.dat.temp$GM)
  
  
  }
  
  ###################################
  ### Predict Yield
  ###################################
  
  Soyyield.predict          <- predict(Soy.parameters[[i]][[Soy.break]], node = 'Yield', 
                                  data = Soyintense.dat.temp, 
                                    method = 'bayes-lw')
  
  ### Soy data updates
  
  Soyintense.dat[[i]][Soyintense.dat[[i]]$Year == year, colnames(Soyintense.dat[[i]]) %in% colnames(Soyintense.dat.temp)] <- Soyintense.dat.temp
  
  
  ### Yield prediction df
  
  Soyintense.dat.temp       <- data.frame(Soyintense.dat[[i]] %>% 
                                            filter(Year == year))[, 2:ncol(Soyintense.dat[[i]])]
  
  
  Soyyield.predict          <- data.frame(Soyyield.predict, Soyintense.dat.temp$OverallANSI)
  

  ##################################################################################
  
  ### Corn Intensification
  
  ##################################################################################
  
  Corn.break                 <- ifelse(year < Cornintense.breaks[[i]][1], 1, 2)
  
  Cornintense.dat.temp       <- data.frame(Cornintense.dat[[i]] %>% 
                                            filter(Year == year))[, 4:ncol(Cornintense.dat[[i]])]
  
  Cornintense.dat.temp       <- Cornintense.dat.temp[, colnames(Cornintense.dat.temp) %in% nodes(Corn.parameters[[i]][[Corn.break]])]
  
  
  ############
  ### Predict Intensification Factors from Price
  ############
  
  temp.arc                  <- arcs(Corn.parameters[[i]][[Corn.break]])
  temp.arc                  <- temp.arc[temp.arc[, 1] %in% c('Price_lag1', 'Expection')]
  temp.arc                  <- temp.arc[temp.arc != 'Expection' & temp.arc != 'Price_lag1' & 
                                          temp.arc != 'Phosphate.intense' & temp.arc != 'Machinery_Dollar']
  temp.arc                  <- temp.arc[temp.arc != 'Acres_Planted']
  temp.arc                  <- unique(temp.arc)

  
  if(length(temp.arc)>0) {
    
    price.update <- list()
    
    for(j in 1:length(temp.arc)) {
      
      price.update[[j]]             <- predict(Corn.parameters[[i]][[Corn.break]], node = temp.arc[j], 
                                                    data = Cornintense.dat.temp, 
                                                    method = 'bayes-lw')
      

    }
    
    for(j in 1:length(temp.arc)) {
      
      ### Constrain irrigation
      
      if(temp.arc[j] == 'irrigatedpct') {
        
        price.update[[j]] <- ifelse(Cornintense.dat.temp$irrigatedpct == 0, 0, unlist(price.update[[j]]))
        
      }
      
    }
    
    for(j in 1:length(price.update)) {
    
    Cornintense.dat.temp[, 
                         which(colnames(Cornintense.dat.temp) == 
                                 temp.arc[j])]  <- price.update[[j]]

    }

  }
  
  ### Constraints on values
  
  Cornintense.dat.temp[, colnames(Cornintense.dat.temp) != 'Expection']      <- data.frame(apply(Cornintense.dat.temp[, colnames(Cornintense.dat.temp) != 'Expection'], 2, function(x) {ifelse(x < 0, 0, x)}))
  
  GM.key                    <- contains('GM', vars = colnames(Cornintense.dat.temp))
  
  if(length(GM.key) > 0) {
  
    for(j in 1:length(GM.key)) {
    
  Cornintense.dat.temp[, GM.key[j]]    <- ifelse(year < 1997, 
                                                0, Cornintense.dat.temp[, GM.key[j]])
  
  Cornintense.dat.temp[, GM.key[j]]    <- ifelse(Cornintense.dat.temp[, GM.key[j]] > 100, 100, Cornintense.dat.temp[, GM.key[j]])
  
    }
  
  }
  
  ###################################
  ### Predict Yield
  ###################################
  
  
  Cornyield.predict          <- predict(Corn.parameters[[i]][[Corn.break]], node = 'Yield', 
                                       data = Cornintense.dat.temp, 
                                       method = 'bayes-lw')
  
  
  ### Corn data updates
  
  Cornintense.dat[[i]][Cornintense.dat[[i]]$Year == year, colnames(Cornintense.dat[[i]]) %in% colnames(Cornintense.dat.temp)] <- Cornintense.dat.temp
  
  
  ### Yield prediction df
  
  Cornintense.dat.temp       <- data.frame(Cornintense.dat[[i]] %>% 
                                            filter(Year == year))[, 2:ncol(Cornintense.dat[[i]])]
  
  
  Cornyield.predict          <- data.frame(Cornyield.predict, Cornintense.dat.temp$OverallANSI)
  
  
  ##################################################################################
  
  ### Soy Extense update
  
  ##################################################################################
  
  
  SoyExtense.temp            <-  data.frame(Soyextense.dat[[i]] %>%
                                        filter(Year == year))[, 4:ncol(Soyextense.dat[[i]])]
  
  
  ##################################################################################
  
  ### Corn Extense update
  
  ##################################################################################
  
  
  CornExtense.temp               <-  data.frame(Cornextense.dat[[i]] %>%
                                                 filter(Year == year))[, 4:ncol(Cornextense.dat[[i]])]
  
  

  
  #####################################################
  
  ### Predict Extensification
  
  #####################################################
  
  ######################
  ### Soy
  ######################
  
  
  Soy.break                   <- ifelse(year < SoyExtense.breaks[[i]][1], 1, 2)
  
  SoyExtense.temp             <- SoyExtense.temp[, colnames(SoyExtense.temp) %in% nodes(SoyExtense.parameters[[i]][[Soy.break]])]
  
  SoyExtense.predict          <- predict(SoyExtense.parameters[[i]][[Soy.break]], node = 'DELTA_Soy', 
                                         data = SoyExtense.temp, 
                                         method = 'bayes-lw')
  
  SoyExtense.dat.temp         <- data.frame(Soyextense.dat[[i]] %>% 
                                              filter(Year == year))[, 2:ncol(Soyextense.dat[[i]])]
  
  SoyExtense.predict          <- data.frame(SoyExtense.predict, SoyExtense.dat.temp$OverallANSI,
                                            SOY_Acres_Planted.y = SoyExtense.dat.temp$SOY_Acres_Planted.y)
  
  
  ####################
  ### Corn
  ####################
  
  Corn.break                   <- ifelse(year < CornExtense.breaks[[i]][1], 1, 2)
  
  CornExtense.temp             <- CornExtense.temp[, colnames(CornExtense.temp) %in% nodes(CornExtense.parameters[[i]][[Corn.break]])]
  
  CornExtense.predict          <- predict(CornExtense.parameters[[i]][[Corn.break]], node = 'DELTA_Corn', 
                                         data = CornExtense.temp, 
                                         method = 'bayes-lw')
  
  CornExtense.dat.temp         <- data.frame(Cornextense.dat[[i]] %>% 
                                              filter(Year == year))[, 2:ncol(Cornextense.dat[[i]])]
  
  CornExtense.predict          <- data.frame(CornExtense.predict, CornExtense.dat.temp$OverallANSI,
                                             Corn_Acres_Planted.x = CornExtense.dat.temp$CORN_Acres_Planted.x)
  
  
  ##################################################################################
  
  ### Cache Data
  
  ##################################################################################
  
  ##########################
  ### Soy
  ##########################
  

  ### Yield
  
  temp.constrain                  <- ifelse(Soyyield.predict$Soyyield.predict < 0, 0, Soyyield.predict$Soyyield.predict)
  
  temp.constrain                  <- ifelse(temp.constrain > 150, 150, temp.constrain)
  
  temp.Soyyield[[i]]              <- temp.constrain

  
  ### DELTA
  
  temp.constrain                  <- SoyExtense.predict$SoyExtense.predict
  
  temp.constrain                  <- ifelse(temp.constrain < 0 & abs(temp.constrain) > SoyExtense.predict$SOY_Acres_Planted.y, 0 - SoyExtense.predict$SOY_Acres_Planted.y, temp.constrain)
  
  temp.constrain                  <- ifelse(temp.constrain > 0, temp.constrain / 1.5, temp.constrain) ### Calibration constraint
  
  temp.constrain                  <- ifelse(temp.constrain > 0 & temp.constrain + SoyExtense.predict$SOY_Acres_Planted.y > 300000, temp.constrain / 2, temp.constrain)
  
  temp.SoyDelta[[i]]              <- temp.constrain
  
  
  ### Extense
  
  temp.constrain                  <- SoyExtense.predict$SOY_Acres_Planted.y + temp.SoyDelta[[i]]
  
  temp.constrain                  <- ifelse(temp.constrain < 0, 0, temp.constrain)
  
  temp.constrain                  <- ifelse(temp.constrain > 600000, 600000, temp.constrain)

  temp.Soyextense[[i]]            <- temp.constrain


  
  ### Production
  
  SoyExpectation.temp            <-  data.frame(Expectation.Soy[[i]] %>%
                                              filter(Year == year))[, 4:ncol(Expectation.Soy[[i]])]
  
  temp.Soyproduction[[i]]        <- unlist(temp.Soyextense[[i]]) * SoyExpectation.temp$Wastage * unlist(temp.Soyyield[[i]])
  
  
  
  ##########################
  ### Corn
  ##########################
  
  
  ### Yield
  
  temp.constrain                  <- ifelse(Cornyield.predict$Cornyield.predict < 0, 0, Cornyield.predict$Cornyield.predict)
  
  temp.constrain                  <- ifelse(temp.constrain > 500, 500, temp.constrain)
  
  temp.Cornyield[[i]]             <- temp.constrain
  
  
  ### DELTA
  
  temp.constrain                  <- CornExtense.predict$CornExtense.predict
  
  temp.constrain                  <- ifelse(temp.constrain < 0 & abs(temp.constrain) > CornExtense.predict$Corn_Acres_Planted.x, 0 - CornExtense.predict$Corn_Acres_Planted.x, temp.constrain)
  
  temp.constrain                  <- ifelse(temp.constrain > 0, temp.constrain / 1.5, temp.constrain)
  
  temp.constrain                  <- ifelse(temp.constrain > 0 & temp.constrain + CornExtense.predict$Corn_Acres_Planted.x > 300000, temp.constrain / 2, temp.constrain)
  
  temp.CornDelta[[i]]             <- temp.constrain
  
  
  ### Extense
  
  temp.constrain                  <- CornExtense.predict$Corn_Acres_Planted.x + temp.CornDelta[[i]]
  
  temp.constrain                  <- ifelse(temp.constrain < 0, 0, temp.constrain)
  
  temp.constrain                  <- ifelse(temp.constrain > 600000, 600000, temp.constrain)
  
  temp.Cornextense[[i]]           <- temp.constrain
  

  
  ### Production
  
  CornExpectation.temp            <-  data.frame(Expectation.Corn[[i]] %>%
                                                  filter(Year == year))[, 4:ncol(Expectation.Corn[[i]])]
  
  temp.Cornproduction[[i]]        <- unlist(temp.Cornextense[[i]]) * CornExpectation.temp$Wastage * unlist(temp.Cornyield[[i]])
  
  

  
  ### Interface
  
  print(paste0(i, ' of ', 6, ' iterations complete.'))
  
  }
  

  ##############################################################################
  
  ### Calculate Price
  
  ##############################################################################
  
  #########################
  
  ### Soy
  
  #########################
  
  
  ##########################################
  ### Update Variables
  ##########################################
  
  
  temp.Soyproduction          <- sum(unlist(temp.Soyproduction))
  
  temp.Soyproduction          <- temp.Soyproduction * Soy.convert[Soy.convert$Year == year, ]$Conversion
  
  Price.temp                  <- Soy.derivative
  
  Price.temp$US.Production    <- temp.Soyproduction

  
  #########################################
  ### Make predictions
  #########################################

    
  Price2                      <- 10 ** predict(Price.Soy.bnfit, node = 'logPrice', data = Price.temp[Price.temp$Year == year, ])
  
  print(Price2)
  
  #########################
  
  ### Corn
  
  #########################
  
  
  ##########################################
  ### Update Variables
  ##########################################
  
  
  temp.Cornproduction            <- sum(unlist(temp.Cornproduction))
  
  temp.Cornproduction            <- temp.Cornproduction * Corn.convert[Corn.convert$Year == year, ]$Conversion
  
  Corn.Price.temp                <- Corn.derivative
  
  Corn.Price.temp$Production     <- temp.Cornproduction
  
  Corn.Price.temp$Prod_Ethanolshare_interact <- Corn.Price.temp$Production * Corn.Price.temp$Ethanol.share.of.total.use

  
  #########################################
  ### Make predictions
  #########################################

  
  CornPrice2                      <- 10 ** predict(Price.Corn.bnfit, 'Price', data = Corn.Price.temp[Corn.Price.temp$Year == year, ])
    
  
  print(CornPrice2)
  
  ################################################################################
  
  ### Calculate DELTA Expection
  
  ################################################################################
  
  ### Corn
  
  for(i in 1:length(Expectation.Corn)) {
  
    
  Expectation.Corn.temp            <- data.frame(Expectation.Corn[[i]] %>%
                                                   filter(Year == year))
  
  Expectation.Corn.temp$Price      <- CornPrice2
  
  Expectation.Corn.temp$Expection2 <-  temp.Cornyield[[i]] * Expectation.Corn.temp$Price * Expectation.Corn.temp$Wastage
  
  
  Expectation.Corn.temp$Difference <-   Expectation.Corn.temp$Expection2 - Expectation.Corn.temp$Expection

  
  ### cache data for updates
  
  temp.CornDifference[[i]]         <- Expectation.Corn.temp$Difference
  
  
  }
  
  
  ### Soy
  
  for(i in 1:length(Expectation.Soy)) {

    
    Expectation.Soy.temp                 <- data.frame(Expectation.Soy[[i]] %>%
                                                         filter(Year == year))
    
    Expectation.Soy.temp$Price           <- Price2

    Expectation.Soy.temp$Expection2      <-  temp.Soyyield[[i]] * Expectation.Soy.temp$Price * Expectation.Soy.temp$Wastage
    
    Expectation.Soy.temp$Difference      <-   Expectation.Soy.temp$Expection2 - Expectation.Soy.temp$Expection

    
    ### cache data for updates
    
    temp.SoyDifference[[i]]              <- Expectation.Soy.temp$Difference
    
  }
  
  
  
  ################################################################################
  
  ### Update Other Variables
  
  ################################################################################
  
  
  
  if((year +1) < 2018) {
  
  ########################################################
  
  ### Update intense
  
  ########################################################
  
  ### Soy
    
  for(i in 1:length(Soyintense.dat)) {
    
    Soyintense.dat[[i]][Soyintense.dat[[i]]$Year == (year+1), ]$Price_lag1     <-  Price2
    
    Soyintense.dat[[i]][Soyintense.dat[[i]]$Year == (year+1), ]$Expection      <-  Soyintense.dat[[i]][Soyintense.dat[[i]]$Year == (year+1), ]$Expection * (1/3 * temp.SoyDifference[[i]])
    
  }


  #for(i in 1:length(Soyintense.dat)) {
  #  
  #  Soyintense.dat[[i]][Soyintense.dat[[i]]$Year == (year+1), ]$Acres_Planted <-  Soyintense.dat[[i]][Soyintense.dat[[i]]$Year == year, ]$Acres_Planted + temp.SoyDelta[[i]]
  #  
  #  Soyintense.dat[[i]][Soyintense.dat[[i]]$Year == (year+1), ]$Acres_Planted <-  ifelse(Soyintense.dat[[i]][Soyintense.dat[[i]]$Year == (year+1), ]$Acres_Planted < 0,
  #                                                                                       0, Soyintense.dat[[i]][Soyintense.dat[[i]]$Year == (year+1), ]$Acres_Planted)
  #                                                                                       
  #}
  
  ### Corn
    
  for(i in 1:length(Cornintense.dat)) {
      
    Cornintense.dat[[i]][Cornintense.dat[[i]]$Year == (year+1), ]$Price_lag1     <-  CornPrice2
      
    Cornintense.dat[[i]][Cornintense.dat[[i]]$Year == (year+1), ]$Expection      <-  Cornintense.dat[[i]][Cornintense.dat[[i]]$Year == (year+1), ]$Expection * (temp.CornDifference[[i]] / 3)
    
    
    }
    
  #for(i in 1:length(Cornintense.dat)) {
  #    
  #  Cornintense.dat[[i]][Cornintense.dat[[i]]$Year == (year+1), ]$Acres_Planted <-  Cornintense.dat[[i]][Cornintense.dat[[i]]$Year == year, ]$Acres_Planted + temp.CornDelta[[i]]
  #    
  #  Cornintense.dat[[i]][Cornintense.dat[[i]]$Year == (year+1), ]$Acres_Planted <-  ifelse(Cornintense.dat[[i]][Cornintense.dat[[i]]$Year == (year+1), ]$Acres_Planted < 0,
  #                                                                                         0, Cornintense.dat[[i]][Cornintense.dat[[i]]$Year == (year+1), ]$Acres_Planted)
  #    
  #  }
  
  ########################################################
  
  ### Update Extense
  
  ########################################################
  
  ### Soy
    
  for(i in 1:length(Soyextense.dat)) {
    
    Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$SOY_Acres_Planted.y <-  Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == year, ]$SOY_Acres_Planted.y + temp.SoyDelta[[i]]
    
    Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$SOY_Acres_Planted.y <-  ifelse(Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$SOY_Acres_Planted.y < 0, 
                                                                                               0, Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$SOY_Acres_Planted.y)
    
  }
  
  for(i in 1:length(Soyextense.dat)) {
    
    Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$Price_lag1     <-  Price2
    
    Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$DELTA_Soy_LAG1 <-  temp.SoyDelta[[i]]
    
  }
  
    
  ### Update GM PCT
    
#  for(i in 1:length(Soyextense.dat)) {
#    
#    Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$Pre_GM.pct   <- Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$SOY_Acres_Planted.y / Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == 1997, ]$SOY_Acres_Planted.y
#    
#    if(year == 1996) {
#      
#      Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$Pre_GM.pct <- 1
#      
#    }
#    
#    Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$Pre_GM.pct <- ifelse(is.na(Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$Pre_GM.pct), 
#                                                                                           0, Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$Pre_GM.pct)
#    
#    Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$Pre_GM.pct <- ifelse(is.infinite(Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$Pre_GM.pct), 
#                                                                                     0, Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$Pre_GM.pct)
#    
#    
#    if(year == 1996) {
#      
#      Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$Pre_GM.pct <- 1
#      
#    }
#    
 # }
    
    
  ### Corn
  
  for(i in 1:length(Cornextense.dat)) {
      
    Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$CORN_Acres_Planted.x <-  Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == year, ]$CORN_Acres_Planted.x + temp.CornDelta[[i]]
      
    Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$CORN_Acres_Planted.x  <-  ifelse(Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$CORN_Acres_Planted.x < 0, 
                                                                                                 0, Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$CORN_Acres_Planted.x)
      
    }
    
  for(i in 1:length(Cornextense.dat)) {
      
    Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$Price_lag1      <-  CornPrice2
      
    Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$DELTA_Corn_LAG1 <-  temp.CornDelta[[i]]
    
    }
    
    ### Update GM PCT
    
#    for(i in 1:length(Cornextense.dat)) {
#      
#      Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$Pre_GM.pct   <- Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$CORN_Acres_Planted.x / Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == 1997, ]$CORN_Acres_Planted.x
#      
#      if(year == 1996) {
#        
#        Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$Pre_GM.pct <- 1
#        
#      }
#      
#      Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$Pre_GM.pct <- ifelse(is.na(Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$Pre_GM.pct), 
#                                                                                             0, Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$Pre_GM.pct)
#      
#      
#      Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$Pre_GM.pct <- ifelse(is.infinite(Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$Pre_GM.pct), 
#                                                                                         0, Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$Pre_GM.pct)
      
      
      
#    }
    
      
  
  #######################################################
  
  ### Update Expectation 
  
  #######################################################
 

    
  ### Soy
    
  for(i in 1:length(Soyextense.dat)) {
    
    Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$EXPECTED.SoyCorn_difference <- Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$EXPECTED.SoyCorn_difference + (temp.SoyDifference[[i]] /3)
    
    Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$EXPECTED.SOYCRP_difference <- Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$EXPECTED.SOYCRP_difference + (temp.SoyDifference[[i]] /3)
    
  }
    

  ### Corn
    
  for(i in 1:length(Cornextense.dat)) {
      
    Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$EXPECTED.SoyCorn_difference <- Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$EXPECTED.SoyCorn_difference + (temp.CornDifference[[i]] /3)
      
    Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$EXPECTED.CORNCRP_difference <- Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$EXPECTED.CORNCRP_difference + (temp.CornDifference[[i]] /3)
      
    }
    

  
  ######################################################################################################
  
  ### Soy - Corn cross influence
  
  ######################################################################################################
  
  SoyDifference.Median  <- lapply(temp.SoyDifference, median)
  CornDifference.Median <- lapply(temp.CornDifference, median)
  
  
  for(i in 1:length(Soyextense.dat)) {
    
    temp2.Soyextense <- Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]
    temp2.Soyextense <- data.frame(OverallANSI = temp2.Soyextense$OverallANSI, Difference = temp.SoyDifference[[i]])
    
    temp2.Cornextense<- Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]
    temp2.Cornextense<- data.frame(OverallANSI = temp2.Cornextense$OverallANSI, Difference = temp.CornDifference[[i]])

    ### Soy
    
    for(j in 1:nrow(temp2.Soyextense)) {
    
      if(temp2.Soyextense$OverallANSI[j] %in% temp2.Cornextense$OverallANSI) {
      
    Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$EXPECTED.SoyCorn_difference[j] <- Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$EXPECTED.SoyCorn_difference[j] -  (temp2.Cornextense[which(temp2.Cornextense$OverallANSI == temp2.Soyextense$OverallANSI[j]), ]$Difference / 3)                                  
    
      } else if(!temp2.Soyextense$OverallANSI[j] %in% temp2.Cornextense$OverallANSI) {
    
    Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$EXPECTED.SoyCorn_difference[j] <- Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$EXPECTED.SoyCorn_difference[j] - (CornDifference.Median[[i]] / 3)
        
      }
    
    }
    
    ### Corn
    
    for(j in 1:nrow(temp2.Cornextense)) {
      
      if(temp2.Cornextense$OverallANSI[j] %in% temp2.Soyextense$OverallANSI) {
        
    Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$EXPECTED.SoyCorn_difference[j] <- Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$EXPECTED.SoyCorn_difference[j] +  (temp2.Soyextense[which(temp2.Soyextense$OverallANSI == temp2.Cornextense$OverallANSI[j]), ]$Difference / 3)                                  
        
      } else if(!temp2.Cornextense$OverallANSI[j] %in% temp2.Soyextense$OverallANSI) {
        
    Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$EXPECTED.SoyCorn_difference[j] <- Cornextense.dat[[i]][Cornextense.dat[[i]]$Year == (year+1), ]$EXPECTED.SoyCorn_difference[j] + (SoyDifference.Median[[i]] / 3)
        
        }
      
      }
    
    
    }
  
  }
  
  
  #################################
  ### Bank Soy results
  #################################
  
  SoyProduction.Results <- c(SoyProduction.Results, temp.Soyproduction)
  SoyIntense.Results    <- c(SoyIntense.Results, temp.Soyyield)
  SoyExtense.Results    <- c(SoyExtense.Results, temp.Soyextense)
  SoyDelta.Results      <- c(SoyDelta.Results, temp.SoyDelta)
  SoyMargin.Results     <- c(SoyMargin.Results, temp.SoyDifference) ### Difference in profit margin on original
  SoyPrice.Results      <- c(SoyPrice.Results, Price2)
  
  
  #################################
  ### Bank Corn results
  #################################
  
  CornProduction.Results <- c(CornProduction.Results, temp.Cornproduction)
  CornIntense.Results    <- c(CornIntense.Results, temp.Cornyield)
  CornExtense.Results    <- c(CornExtense.Results, temp.Cornextense)
  CornDelta.Results      <- c(CornDelta.Results, temp.CornDelta)
  CornMargin.Results     <- c(CornMargin.Results, temp.CornDifference)
  CornPrice.Results      <- c(CornPrice.Results, CornPrice2)
  
  ### Interface
  
  print(year)
  
}



#************************************************************************************************
#################################################################################################

### 4) Bayesian Network parameter updating function for Monte Carlo Simulation

#################################################################################################
#************************************************************************************************


Soy.Bayesian         <- FINAL.Soy
Soy.Bayesian.Extense <- FINAL.SoyExtense

Corn.Bayesian         <- FINAL.Corn
Corn.Bayesian.Extense <- FINAL.CornExtense

###################################################################
### Stanmods objects are lists of bayesian GLMs, created with the Rstanarm library
###################################################################

for(iteration in 1:1000) {

  
  for(mod in 1:length(FINAL.Soy)) {

    
    
    ########################################################
    
    ### Soy Extense updating
    
    ########################################################
    
    
    for(sub.model in 1:length(FINAL.SoyExtense[[mod]])) {
      
      ################
      ### take random sample from Markov Chain Monte Carlo posterior distributions 
      ################
      
      random.pars <- lapply(stanmods.Extense.Soy[[mod]][[sub.model]], as.matrix)
      random.pars <- lapply(random.pars, function(x){apply(x, 2, function(x) {sample(x, size = 1, replace = FALSE)})})
      
      var.coefs   <- unique(unlist(lapply(random.pars, length)))
      
      var.coefs   <- var.coefs[var.coefs != 1]
      
      for(var in 1:length(random.pars)) {
        
        if(length(random.pars[[var]]) != var.coefs) {
          
          random.pars[[var]] <- rep(0, times = var.coefs)
          
        }
        
      }
      
      
      
      
      if(length(stanmods.Extense.Soy[[mod]][[sub.model]]) > 1) {
      
      ### prepare data
        
      pars                                         <- data.frame(random.pars)
      dev.resid                                    <- as.numeric(pars[nrow(pars), ])
      pars                                         <- pars[-nrow(pars), ]
      colnames(pars)                               <- c(0, 1, 2)
      
      ### Insert
      
      Soy.Bayesian.Extense[[mod]][[sub.model]]$DELTA_Soy <- list(coef = as.matrix(pars), sd = dev.resid)
      
      #print(all.equal(names(Soy.Bayesian.Extense[[mod]][[sub.model]]$DELTA_Soy), names(random.pars)[1:length(random.pars)-1]))
      
      
      } else if(length(stanmods.Extense.Soy[[mod]][[sub.model]]) == 1) {
        
        ### prepare data
        
        pars       <- as.numeric(random.pars[-length(random.pars)])
        dev.resid  <- as.numeric(random.pars[length(random.pars)])
        
        ### Insert
        
        Soy.Bayesian.Extense[[mod]][[sub.model]]$DELTA_Soy <- list(coef = pars, sd = dev.resid)
        
        #print(all.equal(Soy.Bayesian.Extense[[mod]][[sub.model]]$DELTA_Soy$coefficients, as.matrix(pars)))
        
        
      }
      
      
      
      
      
    }
    
    
    ##########################################################
    
    ### Corn Extense Updating
    
    ##########################################################
    
    for(sub.model in 1:length(FINAL.CornExtense[[mod]])) {
      
      #coef.dif[[mod]][[sub.model]] <- list()
      
      #print(sub.model)
      
      
      ### random sample
      
      random.pars <- lapply(stanmods.Extense.Corn[[mod]][[sub.model]], as.matrix)
      random.pars <- lapply(random.pars, function(x){apply(x, 2, function(x) {sample(x, size = 1, replace = FALSE)})})
      
      var.coefs   <- unique(unlist(lapply(random.pars, length)))
      
      var.coefs   <- var.coefs[var.coefs != 1]
      
      for(var in 1:length(random.pars)) {
        
        if(length(random.pars[[var]]) != var.coefs) {
          
          random.pars[[var]] <- rep(0, times = var.coefs)
          
        }
        
      }
      
      
      if(length(stanmods.Extense.Corn[[mod]][[sub.model]]) > 1) {
        
        ### prepare data
        
        pars                                         <- data.frame(random.pars)
        dev.resid                                    <- as.numeric(pars[nrow(pars), ])
        pars                                         <- pars[-nrow(pars), ]
        colnames(pars)                               <- c(0, 1, 2)
        
        ### Insert
        
        Corn.Bayesian.Extense[[mod]][[sub.model]]$DELTA_Corn <- list(coef = as.matrix(pars), sd = dev.resid)
        
        print(all.equal(Corn.Bayesian.Extense[[mod]][[sub.model]]$DELTA_Corn$coefficients, as.matrix(pars)))
        
      } else if(length(stanmods.Corn.Extense[[mod]][[sub.model]] == 1)) {
        
        
        ### prep
        
        pars       <- as.numeric(random.pars[-length(random.pars)])
        dev.resid  <- as.numeric(random.pars[length(random.pars)])
        
        ### insert
        
        Corn.Bayesian.Extense[[mod]][[sub.model]]$DELTA_Corn <- list(coef = as.matrix(pars), sd = dev.resid)
        
        #print(all.equal(names(Corn.Bayesian.Extense[[mod]][[sub.model]]$DELTA_Corn), names(random.pars)[1:length(random.pars)-1]))
        
      }
      
      
      
    }
    
    
    ############################################################
    
    ### Soy Intense updating
    
    ############################################################
    
    
    for(sub.model in 1:length(FINAL.Soy[[mod]])) {
      
      #coef.dif[[mod]][[sub.model]] <- list()
      
      #print(sub.model)
      
      
      ### random sample
      
      random.pars <- as.matrix(stanmods.Soy[[mod]][[sub.model]])
      
      #coef.dif[[mod]][sub.model] <- (Metrics::rmse(apply(random.pars, 2, mean)[1:ncol(random.pars)-1], FINAL.Soy[[mod]][[sub.model]]$Yield$coefficients))
      
      random.pars <- apply(random.pars, 2, function(x) {sample(x, size = 1, replace = FALSE)}) #set replace to false
      

      ### prepare data
      
      pars       <- as.numeric(random.pars[-length(random.pars)])
      dev.resid  <- as.numeric(random.pars[length(random.pars)])
      
      ### insert
      
      Soy.Bayesian[[mod]][[sub.model]]$Yield <- list(coef = pars, sd = dev.resid)
      
      #print(all.equal(names(Soy.Bayesian[[mod]][[sub.model]]$Yield), names(random.pars)[1:length(random.pars)-1]))
      
    }
    
    
    
    ##################################
    
    ### Corn intense updating
    
    ##################################
    
    
    for(sub.model in 1:length(FINAL.Corn[[mod]])) {
      
      #coef.dif[[mod]][[sub.model]] <- list()
      
      #print(sub.model)
      
      
      ## sample
      
      random.pars <- as.matrix(stanmods.Corn[[mod]][[sub.model]])
      
      #coef.dif[[mod]][sub.model] <- (Metrics::rmse(apply(random.pars, 2, mean)[1:ncol(random.pars)-1], FINAL.Corn[[mod]][[sub.model]]$Yield$coefficients))
      
      random.pars <- apply(random.pars, 2, function(x) {sample(x, size = 1, replace = FALSE)}) #set replace to false
      
      ## prepare data
      
      pars       <- as.numeric(random.pars[-length(random.pars)])
      dev.resid  <- as.numeric(random.pars[length(random.pars)])
      
      
      Corn.Bayesian[[mod]][[sub.model]]$Yield <- list(coef = pars, sd = dev.resid)
      
      #print(all.equal(names(Corn.Bayesian[[mod]][[sub.model]]$Yield), names(random.pars)[1:length(random.pars)-1]))
      
    }
    
    
    ###########################################################
    
    ### Do Soy Price updating
    
    ###########################################################
    
    
    for(sub.model in 1:length(FINAL.Soy[[mod]])) {
      

      
      for(Pricenode in 1:length(FINAL.Soy[[mod]][[sub.model]])) {
        
        #print(sub.model)
        
        
        if(Pricenode > length(stanmods.Price.Soy[[mod]][[sub.model]])) {
          
          next()
        }
        
        
        if(length(stanmods.Price.Soy[[mod]][[sub.model]][[Pricenode]]) > 0) {
          
          
          random.pars <- as.matrix(stanmods.Price.Soy[[mod]][[sub.model]][[Pricenode]])
          
          random.pars <- apply(random.pars, 2, function(x) {sample(x, size = 1, replace = FALSE)}) #set replace to false
          
          
          if(mod == 1 & sub.model == 2 & Pricenode == 6) {
            
            
            random.pars[length(random.pars)+1] <- 0 
            
            names(random.pars)[length(random.pars)] <- 'Expection'
            
            random.pars <- random.pars[c(1, 2, length(random.pars), 3, 4)]
            
            
          }
          
          
          if(mod == 1 & sub.model == 2 & Pricenode == 7) {
            
            
            random.pars[length(random.pars)+1] <- 0 
            
            names(random.pars)[length(random.pars)] <- 'Expection'
            
            random.pars <- random.pars[c(1:3, length(random.pars), 4, 5, 6)]
            
            
          }
          
          
          if(mod == 3 & sub.model == 2 & Pricenode == 4) {
            
            random.pars[length(random.pars)+1] <- 0 
            
            names(random.pars)[length(random.pars)] <- 'Expection'
            
            random.pars <- random.pars[c(1:2, length(random.pars), 3)]
            
          }
          

          ### Prepare data
          
          
          pars       <- as.numeric(random.pars[-length(random.pars)])
          dev.resid  <- as.numeric(random.pars[length(random.pars)])
          index      <- FINAL.Soy[[mod]][[sub.model]][[Pricenode]]$node
          
          ### Insert
          
          Soy.Bayesian[[mod]][[sub.model]][[`index`]] <- list(coef = pars, sd = dev.resid)
          
          #print(all.equal(names(Soy.Bayesian[[mod]][[sub.model]][[`index`]]), names(random.pars)[1:length(random.pars)-1]))
          
          
        }
        
        
      }
      
    }
    
    
    #################################
    
    ### Corn Price Updating
    
    #################################
    
    
    for(sub.model in 1:length(FINAL.Corn[[mod]])) {
      
      
      
      for(Pricenode in 1:length(FINAL.Corn[[mod]][[sub.model]])) {
        
        #print(sub.model)
        
        if(Pricenode > length(stanmods.Price.Corn[[mod]][[sub.model]])) {
          
          next()
        }
        
        if(length(stanmods.Price.Corn[[mod]][[sub.model]][[Pricenode]]) > 0) {
          
          random.pars <- as.matrix(stanmods.Price.Corn[[mod]][[sub.model]][[Pricenode]])
          
          random.pars <- apply(random.pars, 2, function(x) {sample(x, size = 1, replace = FALSE)}) #set replace to false
          
          
          
          pars       <- as.numeric(random.pars[-length(random.pars)])
          dev.resid  <- as.numeric(random.pars[length(random.pars)])
          index      <- FINAL.Corn[[mod]][[sub.model]][[Pricenode]]$node
          
          Corn.Bayesian[[mod]][[sub.model]][[`index`]] <- list(coef = pars, sd = dev.resid)
          
        }
        
        #print(names(Soy.Bayesian[[mod]][[sub.model]]$Yield$coefficients) ==  names(random.pars)[1:length(random.pars)-1])
        
      }
      
    }
    
    
  }



