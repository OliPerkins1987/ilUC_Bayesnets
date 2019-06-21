

library(plyr)
library(dplyr)
library(bnlearn)
library(rstanarm)
library(Metrics)


#############################################################################################

##### Load data

#############################################################################################

### 1) Intensification

setwd('E:/Modules/Dissertation/Data/Model Runs/Parameter Learning/Intense')

load('Intense_parametersv21806.RData')

SoyMedian.score <- lapply(SoyMedian.score, function(x) {x$Breaks})
CornMedian.score<- lapply(CornMedian.score, function(x) {x$Breaks})

### 2) Price

setwd('E:/Modules/Dissertation/Data/FinalCsvs/Price')

Price.Corn <- read.csv('Corn_price.csv')
Price.Soy  <- read.csv('Soy_price.csv')

### 3) Extensification

setwd('E:/Modules/Dissertation/Data/Model Runs/Parameter Learning/Extense')

load('Extense_Parameters1806v2.RData')

DELTA_Corn.res <- lapply(DELTA_Corn.res, function(x) {x$Breaks})
DELTA_Soy.res  <- lapply(DELTA_Soy.res, function(x) {x$Breaks})

remove(cl, EDISON.convert)

### 4) Profit Loss model

setwd('E:/Modules/Dissertation/Data/FinalCsvs/Economic')

Corn.Economic <- read.csv('Corn_economic2.csv')
Soy.Economic  <- read.csv('Soy_economic2.csv')

Corn.Economic <- split(Corn.Economic, Corn.Economic$USDAagregion)
Soy.Economic  <- split(Soy.Economic, Soy.Economic$USDAagregion)
Soy.Economic  <- Soy.Economic[1:6]

#remove(d, cl, Soy, Corn, Soy.counts, Corn.complete, Soy.complete, Corn.agregion, Soy.agregion)



###############################################################################

### Price linear model

###############################################################################

### Soy

Soy.derivative <- Price.Soy

Soy.derivative$USprod_Subtract_China       <- Soy.derivative$US.Production - Soy.derivative$US.China.Exports
Soy.derivative$Chinaimport_Subtract_Brazil <- Soy.derivative$Total.China.Imports - Soy.derivative$Brazil.China.Exports
Soy.derivative$USprod_Subtract_Biodiesel   <- Soy.derivative$US.Production - Soy.derivative$US.Biodiesel.Production

#Soy.derivative <- data.frame(Soy.derivative %>% dplyr::select(1, 2, 4, 11, 13:15))

fit.Soy2        <- lm(log(Price + 2) ~., data = (Soy.derivative %>% filter(Year > 2009))[, c(2, 11,15)])
fit.Soy         <- lm(Price ~., data = (Soy.derivative %>% filter(Year <= 2009))[, c(2, 13, 14)])

### Corn

Corn.derivative<- Price.Corn

Corn.derivative$Production_Subtract_Feed    <- Corn.derivative$Production - Corn.derivative$Feed.and.residual.use
Corn.derivative$Production_Subtract_Ethanol <- Corn.derivative$Production - (Corn.derivative$Ethanol.share.of.total.use / 100 * Corn.derivative$Production) 
Corn.derivative$Production_Subtract_Exports <- Corn.derivative$Production - Corn.derivative$Exports 

#Corn.derivative <- data.frame(Corn.derivative %>% dplyr::select(1, 2, 4, 5, 12:15))

fit.Corn        <- lm(log(Price) ~. , data = Corn.derivative[, -c(6, 8)])


###############################################################################

### County.key

###############################################################################

Corn.ANSIkey <- rbind.fill(Corn.Economic)$OverallANSI
Corn.ANSIkey <- Corn.ANSIkey[Corn.ANSIkey %in% rbind.fill(Corn.train)$OverallANSI]
Corn.ANSIkey <- Corn.ANSIkey[Corn.ANSIkey %in% rbind.fill(CornExtense.train)$OverallANSI]
Corn.ANSIkey <- unique(Corn.ANSIkey)

Soy.ANSIkey <- rbind.fill(Soy.Economic)$OverallANSI
Soy.ANSIkey <- Soy.ANSIkey[Soy.ANSIkey %in% rbind.fill(Soy.train)$OverallANSI]
Soy.ANSIkey <- Soy.ANSIkey[Soy.ANSIkey %in% rbind.fill(SoyExtense.train)$OverallANSI]
Soy.ANSIkey <- unique(Soy.ANSIkey)

################################################################################################

### Set up function variables

################################################################################################


Soyintense.dat <- Soy.train
Cornintense.dat<- Corn.train

Soyextense.dat <- SoyExtense.train
Cornextense.dat<- CornExtense.train

Soyintense.breaks <- SoyMedian.score
Cornintense.breaks<- CornMedian.score

SoyExtense.breaks <- DELTA_Soy.res
CornExtense.breaks<- DELTA_Corn.res

for(i in 1:length(Corn.Economic)) {

  Corn.Economic[[i]] <- Corn.Economic[[i]] %>% filter(!OverallANSI %in% ANSI.key)
  
  Soy.Economic[[i]]  <- Soy.Economic[[i]] %>% filter(!OverallANSI %in% ANSI.Soy)
  
  Corn.Economic[[i]] <- Corn.Economic[[i]] %>% filter(OverallANSI %in% Corn.ANSIkey)
  
  Soy.Economic[[i]]  <- Soy.Economic[[i]] %>% filter(OverallANSI %in% Soy.ANSIkey)
  
  Cornintense.dat[[i]]    <- Cornintense.dat[[i]] %>% filter(OverallANSI %in% Corn.ANSIkey)
  
  Soyintense.dat[[i]]<- Soyintense.dat[[i]] %>% filter(OverallANSI %in% Soy.ANSIkey)
  
  Cornextense.dat[[i]]    <- Cornextense.dat[[i]] %>% filter(OverallANSI %in% Corn.ANSIkey)
  
  Soyextense.dat[[i]]     <- Soyextense.dat[[i]] %>% filter(OverallANSI %in% Soy.ANSIkey)
  
  
}
  

### remove dud for the run

remove(Soymedian.test, Cornmedian.test, SoyNA.test, SoyNA.train, 
       CornNA.test, CornNA.train, Cornmedian.train, Soymedian.train, 
       Soy.test.Extense, Soy.split.Extense, Soy.train.Extense, Corn.calculate)


################################################################################################

### Make Expectation frame

################################################################################################

slideFunct <- function(dat, window, step){
  total <- length(dat)
  spots <- seq(from=1 + window, to=(total), by=step)
  result <- vector(length = length(spots))
  for(i in 4:28){
    result[c(i-window)] <- mean(dat[c((i-window):(i - 1))], na.rm = TRUE)
  }
  return(result)
}


create.expect <- function(data) {
  
  data.copy     <- data    
  
  data.copy$teg <- 1:nrow(data.copy)
  data.copy$teg <- ceiling(data.copy$teg / 28)
  
  temp <- split(data.copy, data.copy$teg)
  temp <- lapply(temp, function(x) {
    
    slideFunct(x$Yield * x$Price * x$Wastage, window = 3, step = 1)
    
  })
  
  temp <- lapply(temp, function(x) {
    
    c(rep(x[1], times =3),x)
    
  })
  
  temp           <- unlist(temp)
  
  data$Expection <- temp
  
  return(data)
  
}


Expectation.Corn    <-  lapply(Corn.Economic, function(x) {x$Yield})
Expectation.Year    <-  lapply(Corn.Economic, function(x) {x$Year})
Expectation.ANSI    <-  lapply(Corn.Economic, function(x) {x$OverallANSI})
Expectation.Agregion<- lapply(Corn.Economic, function(x) {x$USDAagregion})
Expectation.Price   <-  lapply(Corn.Economic, function(x) {x$CORN..GRAIN...PRICE.RECEIVED..MEASURED.IN.....BU})
Expectation.Wastage <-  lapply(Corn.Economic, function(x) {x$Wastage})
Expectation.Subsidy <-  lapply(Corn.Economic, function(x) {x$Total.subsidy})
Expectation.CornCRP <-  lapply(Cornextense.dat, function(x) {x$EXPECTED.CORNCRP_difference})
Expectation.CornCosts<-  lapply(Corn.Economic, function(x) {x$Costs})


for(i in 1:length(Expectation.Corn)) {
  
  Expectation.Corn[[i]]    <-  data.frame(Year = Expectation.Year[[i]], OverallANSI = Expectation.ANSI[[i]],
                                          USDAagregion = Expectation.Agregion[[i]],
                                          Yield = Expectation.Corn[[i]], Price = Expectation.Price[[i]],
                                          Wastage = Expectation.Wastage[[i]], Subsidy = Expectation.Subsidy[[i]],
                                          Expectation.CornCRP = Expectation.CornCRP[[i]], 
                                          Costs = Expectation.CornCosts[[i]])
  
}


Expectation.Soy     <-   lapply(Soy.Economic, function(x) {x$Yield})
Expectation.Year    <-  lapply(Soy.Economic, function(x) {x$Year})
Expectation.ANSI    <-  lapply(Soy.Economic, function(x) {x$OverallANSI})
Expectation.Agregion<- lapply(Soy.Economic, function(x) {x$USDAagregion})
Expectation.Price   <-   lapply(Soy.Economic, function(x) {x$SOYBEANS...PRICE.RECEIVED..MEASURED.IN.....BU})
Expectation.Wastage <-  lapply(Soy.Economic, function(x) {x$Wastage})
Expectation.Subsidy <-  lapply(Soy.Economic, function(x) {x$Total.subsidy})
Expectation.SoyCRP  <-   lapply(Soyextense.dat, function(x) {x$EXPECTED.SOYCRP_difference})
Expectation.SoyCosts<-  lapply(Soy.Economic, function(x) {x$Costs})

for(i in 1:length(Expectation.Soy)) {
  
  Expectation.Soy[[i]]    <-  data.frame(Year = Expectation.Year[[i]], OverallANSI = Expectation.ANSI[[i]],
                                         USDAagregion = Expectation.Agregion[[i]],
                                         Yield = Expectation.Soy[[i]], Price = Expectation.Price[[i]],
                                         Wastage = Expectation.Wastage[[i]], Subsidy = Expectation.Subsidy[[i]],
                                         Expectation.SoyCRP = Expectation.SoyCRP[[i]], 
                                         Costs = Expectation.SoyCosts[[i]])
  
}


Expectation.Soy <- rbind.fill(Expectation.Soy) %>% split(rbind.fill(Expectation.Soy)$OverallANSI) %>% 
                    lapply(create.expect) %>% rbind.fill() 

Expectation.Corn <- rbind.fill(Expectation.Corn) %>% split(rbind.fill(Expectation.Corn)$OverallANSI) %>% 
  lapply(create.expect) %>% rbind.fill()

Expectation.Soy  <- split(Expectation.Soy, Expectation.Soy$USDAagregion)
Expectation.Corn <- split(Expectation.Corn, Expectation.Corn$USDAagregion)

for(i in 1:length(Expectation.Soy)) {

Expectation.Soy[[i]]$Difference   <- 0
Expectation.Corn[[i]]$Difference <- 0 

}
  

remove(Expectation.Agregion, Expectation.ANSI, Expectation.CornCRP, Expectation.Price, 
       Expectation.SoyCRP, Expectation.Subsidy, Expectation.Wastage, Expectation.Year)



###############################################################################################

### Production Conversion

###############################################################################################

Corn.convert <- rbind(rbind.fill(Corn.test), rbind.fill(Corn.train))
Corn.convert$Production <- Corn.convert$Yield * Corn.convert$Acres_Planted
Corn.convert <- data.frame(Corn.convert %>% group_by(Year) %>% summarise(Production = sum(Production)))
Corn.convert$Conversion <- Price.Corn$Production / Corn.convert$Production

Soy.convert <- rbind(rbind.fill(Soy.test), rbind.fill(Soy.train))
Soy.convert$Production <- Soy.convert$Yield * Soy.convert$Acres_Planted
Soy.convert <- data.frame(Soy.convert %>% group_by(Year) %>% summarise(Production = sum(Production)))
Soy.convert$Conversion <- Price.Soy$US.Production / Soy.convert$Production




#***********************************************************************************************

################################################################################################

### Model function

################################################################################################

#***********************************************************************************************

SoyProduction.Results <- list()
SoyIntense.Results    <- list()
SoyExtense.Results    <- list()
SoyPrice.Results      <- list()

CornProduction.Results <- list()
CornIntense.Results    <- list()
CornExtense.Results    <- list()
CornPrice.Results      <- list()


for(year in 1990:2017) {

  
  temp.Soyproduction  <- list()
  temp.Soyyield       <- list() 
  temp.Soyextense     <- list()
  temp.SoyDifference  <- list()
  
  temp.Cornproduction  <- list()
  temp.Cornyield       <- list() 
  temp.Cornextense     <- list()
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
  temp.arc                  <- temp.arc[temp.arc[, 1] == 'Expection']
  temp.arc                  <- temp.arc[temp.arc != 'Expection']
  
  if(length(temp.arc)>0) {
    
    for(j in 1:length(temp.arc)) {
    
    Soyintense.dat.temp[, 
          which(colnames(Soyintense.dat.temp) == 
                  temp.arc[j])]                    <- predict(Soy.parameters[[i]][[Soy.break]], node = temp.arc[j], 
                                                        data = Soyintense.dat.temp, 
                                                             method = 'bayes-lw')
    
    print(j)
    
    }
      
  }
  
  
  #######################
  ### Constraints on values
  #######################
  
  Soyintense.dat.temp       <- data.frame(apply(Soyintense.dat.temp, 2, function(x) {ifelse(x < 0, 0, x)}))
  
  if('GM' %in% colnames(Soyintense.dat.temp)) {
  
  Soyintense.dat.temp$GM    <- ifelse(year < 1997, 0, Soyintense.dat.temp$GM)
  
  }
  
  ###################################
  ### Predict Yield
  ###################################
  
  Soyyield.predict          <- predict(Soy.parameters[[i]][[Soy.break]], node = 'Yield', 
                                  data = Soyintense.dat.temp, 
                                    method = 'bayes-lw')
  
  Soyintense.dat.temp       <- data.frame(Soyintense.dat[[i]] %>% 
                                            filter(Year == year))[, 2:ncol(Soyintense.dat[[i]])]
  
  
  Soyyield.predict          <- data.frame(Soyyield.predict, Soyintense.dat.temp$OverallANSI)
  
  
  #################################
  ### Soy profit loss update
  #################################
  
  
  Soyprofit.temp                <- data.frame(Soy.Economic[[i]] %>%
                                  filter(Year == year))[, 2:ncol(Soy.Economic[[i]])]
  
  Soyprofit.temp$Yield          <- Soyyield.predict$Soyyield.predict

  Soyprofit.temp$Costs          <- Soyprofit.temp$Seed + Soyprofit.temp$Chemicals + Soyprofit.temp$Fertiliser +
                                      Soyprofit.temp$Repairs + Soyprofit.temp$Customservices + Soyprofit.temp$Overhead
  
  Soyprofit.temp$Total.subsidy  <- Soyprofit.temp$ACRO_CO.Subsidy +
                                    Soyprofit.temp$Insurance + Soyprofit.temp$Generic_subsidy
  
  Soyprofit.temp$Margin         <- (Soyprofit.temp$Yield * Soyprofit.temp$SOYBEANS...PRICE.RECEIVED..MEASURED.IN.....BU *
                                    Soyprofit.temp$Wastage) - Soyprofit.temp$Costs + Soyprofit.temp$Total.subsidy
  
  
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
  temp.arc                  <- temp.arc[temp.arc[, 1] == 'Expection']
  temp.arc                  <- temp.arc[temp.arc != 'Expection']
  
  
  if(length(temp.arc)>0) {
    
    for(j in 1:length(temp.arc)) {
      
      Cornintense.dat.temp[, 
                          which(colnames(Cornintense.dat.temp) == 
                                  temp.arc[j])]                    <- predict(Corn.parameters[[i]][[Corn.break]], node = temp.arc[j], 
                                                                              data = Cornintense.dat.temp, 
                                                                              method = 'bayes-lw')
      
      print(j)
      
    }
    
  }
  
  Cornintense.dat.temp       <- data.frame(apply(Cornintense.dat.temp, 2, function(x) {ifelse(x < 0, 0, x)}))
  
  GM.key                    <- contains('GM', vars = colnames(Cornintense.dat.temp))
  
  if(length(GM.key) > 0) {
  
    for(j in 1:length(GM.key)) {
    
  Cornintense.dat.temp[, GM.key[j]]    <- ifelse(year < 1997, 
                                                0, Cornintense.dat.temp[, GM.key[j]])
  
    }
  
  }
  
  ###################################
  ### Predict Yield
  ###################################
  
  
  Cornyield.predict          <- predict(Corn.parameters[[i]][[Soy.break]], node = 'Yield', 
                                       data = Cornintense.dat.temp, 
                                       method = 'bayes-lw')
  
  Cornintense.dat.temp       <- data.frame(Cornintense.dat[[i]] %>% 
                                            filter(Year == year))[, 2:ncol(Cornintense.dat[[i]])]
  
  
  Cornyield.predict          <- data.frame(Cornyield.predict, Cornintense.dat.temp$OverallANSI)
  
  
  ##################################################################################
  
  ### Soy Extense update
  
  ##################################################################################
  
  
  SoyExtense.temp               <-  data.frame(Soyextense.dat[[i]] %>%
                                        filter(Year == year))[, 4:ncol(Soyextense.dat[[i]])]
  
  
  ######################################################
  ### Update the expectation frame to calculate
  ######################################################
  
  
  Expectation.Soy.temp            <- data.frame(Expectation.Soy[[i]] %>%
                                        filter(Year == year))
  
  Expectation.Soy.temp$Gross      <-  Soyyield.predict$Soyyield.predict * SoyExtense.temp$Price
  
  Expectation.Soy.temp$Expection2 <-   Expectation.Soy.temp$Gross - Expectation.Soy.temp$Costs + Expectation.Soy.temp$Subsidy
  
  Expectation.Soy.temp$Difference <-   Expectation.Soy.temp$Expection2 - Expectation.Soy.temp$Expection
  

  #####################################################
  
  ### Predict Extensification
  
  #####################################################
  
  Soy.break                   <- ifelse(year < SoyExtense.breaks[[i]][1], 1, 2)
  
  SoyExtense.temp             <- SoyExtense.temp[, colnames(SoyExtense.temp) %in% nodes(SoyExtense.parameters[[i]][[Soy.break]])]
  
  SoyExtense.predict          <- predict(SoyExtense.parameters[[i]][[Soy.break]], node = 'DELTA_Soy', 
                                       data = SoyExtense.temp, 
                                       method = 'bayes-lw')
  
  SoyExtense.dat.temp         <- data.frame(Soyextense.dat[[i]] %>% 
                                            filter(Year == year))[, 2:ncol(Soyintense.dat[[i]])]
  
  SoyExtense.predict          <- data.frame(SoyExtense.predict, Soyintense.dat.temp$OverallANSI)
  
  
  ##########################################
  ### Cache Data
  ##########################################
  
  ### Production
  
  temp.Soyproduction[[i]]        <- (SoyExtense.temp$SOY_Acres_Planted.y + SoyExtense.predict$SoyExtense.predict) * Soyyield.predict$Soyyield.predict
  
  ### Yield
  
  temp.Soyyield[[i]]             <- Soyyield.predict$Soyyield.predict
  
  ### Extense
    
  temp.Soyextense[[i]]           <- SoyExtense.predict$SoyExtense.predict
    
  temp.SoyDifference[[i]]        <- Expectation.Soy.temp$Difference
  
  
  ### Interface
  
  print(paste0(i, ' of ', 6, ' iterations complete.'))
  
  }
  

  ##############################################################################
  
  ### Calculate Price
  
  ##############################################################################
  
  ##########################################
  ### Update Variables
  ##########################################
  
  
  temp.Soyproduction          <- sum(unlist(temp.Soyproduction))
  
  temp.Soyproduction          <- temp.Soyproduction * Soy.convert[Soy.convert$Year == year, ]$Conversion
  
  Price.temp                  <- Soy.derivative
  
  Price.temp$US.Production    <- temp.Soyproduction
  
  Price.temp$USprod_Subtract_China       <- Price.temp$US.Production - Price.temp$US.China.Exports
  Price.temp$Chinaimport_Subtract_Brazil <- Price.temp$Total.China.Imports - Price.temp$Brazil.China.Exports
  Price.temp$USprod_Subtract_Biodiesel   <- Price.temp$US.Production - Price.temp$US.Biodiesel.Production
  
  
  #########################################
  ### Make predictions
  #########################################
  
  
  if(year <= 2009) {
  
  Price2                      <- predict.lm(fit.Soy, newdata = Price.temp[Price.temp$Year == year, ])
  
  } else if(year > 2009) {
    
  Price2                      <- exp(predict.lm(fit.Soy2, newdata = Price.temp[Price.temp$Year == year, ]))
    
  }
  
  
  ################################################################################
  
  ### Update Variables
  
  ################################################################################
  
  
  
  if((year +1) < 2018) {
  
  ########################################################
  
  ### Update intense
  
  ########################################################
  
  for(i in 1:length(Soyintense.dat)) {
    
    Soyintense.dat[[i]][Soyintense.dat[[i]]$Year == (year+1), ]$Expection     <-  Price2
    
  }

  for(i in 1:length(Soyintense.dat)) {
    
    Soyintense.dat[[i]][Soyintense.dat[[i]]$Year == (year+1), ]$Acres_Planted <-  Soyintense.dat[[i]][Soyintense.dat[[i]]$Year == (year+1), ]$Acres_Planted + temp.Soyextense[[i]]
    
    Soyintense.dat[[i]][Soyintense.dat[[i]]$Year == (year+1), ]$Acres_Planted <-  ifelse(Soyintense.dat[[i]][Soyintense.dat[[i]]$Year == (year+1), ]$Acres_Planted < 0,
                                                                                         0, Soyintense.dat[[i]][Soyintense.dat[[i]]$Year == (year+1), ]$Acres_Planted)
                                                                                         
  }
  
  ########################################################
  
  ### Update Extense
  
  ########################################################
  
  for(i in 1:length(Soyextense.dat)) {
    
    Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$SOY_Acres_Planted.y <-  Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$SOY_Acres_Planted.y + temp.Soyextense[[i]]
    
    Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$SOY_Acres_Planted.y <-  ifelse(Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$SOY_Acres_Planted.y < 0, 
                                                                                               0, Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$SOY_Acres_Planted.y)
    
  }
  
  for(i in 1:length(Soyextense.dat)) {
    
    Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$Price   <-  Price2
    
  }
  
  
  #######################################################
  
  ### Update Expectation 
  
  #######################################################
  
  for(i in 1:length(Soyextense.dat)) {
    
    Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$EXPECTED.SoyCorn_difference <- Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$EXPECTED.SoyCorn_difference + (temp.SoyDifference[[i]] /3)
    
    Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$EXPECTED.SOYCRP_difference <- Soyextense.dat[[i]][Soyextense.dat[[i]]$Year == (year+1), ]$EXPECTED.SOYCRP_difference + (temp.SoyDifference[[i]] /3)
    
  }
    
  for(i in 1:length(Soyintense.dat)) {
      
    Soyintense.dat[[i]][Soyintense.dat[[i]]$Year == (year+1), ]$Expection <- Price2 ### Currently a bug; Expection is actually Price
      

    }
  
  
  }
    
  SoyProduction.Results <- c(SoyProduction.Results, temp.Soyproduction)
  SoyIntense.Results    <- c(SoyIntense.Results, temp.Soyyield)
  SoyExtense.Results    <- c(SoyExtense.Results, temp.Soyextense)
  SoyPrice.Results      <- c(SoyPrice.Results, Price2)
  
  ### Interface
  
  print(year)
  
}



###########################################################################

## Analyse outputs

###########################################################################


### Intense - smooths yield into a more linear trend, less extreme growth

SoyIntense        <- unlist(lapply(SoyIntense.Results, mean))
SoyIntense        <- split(SoyIntense, factor(rep(1:28, each  = 6)))
SoyIntense        <- lapply(SoyIntense, mean)
SoyIntense.actual <- rbind.fill(Soyintense.dat) %>% group_by(Year) %>% summarise(Yield = mean(Yield)) 
SoyIntense.actual$Predict <- unlist(SoyIntense)


### Extense - similarly it smooths out extense, but the actual sum is roughly the same

SoyExtense        <- unlist(lapply(SoyExtense.Results, mean))
SoyExtense        <- split(SoyExtense, factor(rep(1:28, each  = 6)))
SoyExtense        <- lapply(SoyExtense, mean)
SoyExtense.actual <- rbind.fill(Soyextense.dat) %>% group_by(Year) %>% summarise(Delta_Soy = mean(DELTA_Soy)) 
SoyExtense.actual$Predict <- unlist(SoyExtense)


### Production

SoyProduction        <- unlist(SoyProduction.Results)
#SoyProduction        <- split(SoyProduction, factor(rep(1:28, each  = 6)))
#SoyProduction        <- lapply(SoyProduction, mean)
Price.Soy$Predict <- unlist(SoyProduction)

