
###################################################################################
#
#   Script imports and cleans data for further analysis and modelling
#
###################################################################################


library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(raster)
library(rgdal)
library(zoo)



##################################################################################

### Region shapefiles 

##################################################################################

  ### USA

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/cb_2017_us_county_5m')
counties                  <- readOGR('cb_2017_us_county_5m.shp')
counties$code             <- paste0(as.character(counties$STATEFP), as.character(counties$COUNTYFP))

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/US States')
states                   <- readOGR('cb_2017_us_state_5m.shp')


setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Agricultural districts US')
county_to_ag_region      <- read.csv('county to agricultural region key.csv')
county_to_ag_region      <- county_to_ag_region[, c(1, 2,4)]
counties_key_list        <- as.list(county_to_ag_region$ERS.resource.region)
names(counties_key_list) <- county_to_ag_region$ï..Fips
counties_key_list        <- list2env(counties_key_list, hash = TRUE)

  ### function to map counties to ag regions

map_agregion             <- function(x) { 
  
  t <- vector(length = length(x$OverallANSI))
                              
  for(i in 1:length(x$OverallANSI)) {
    
    try(
    t[i] <- county_to_ag_region$ERS.resource.region[which(x$OverallANSI[i] == as.character(county_to_ag_region$ï..Fips))]
    )
                              }
  
  return(t)
  
  }


##################################################################################

  ### 1) Soy

##################################################################################


setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Soy')

### USA


  ### a) planted area

Soy_planted              <- read.csv('Soy planted area_1990_2017.csv') 
Soy_planted$County.ANSI  <- formatC(Soy_planted$County.ANSI, width = 3, format = "d", flag = "0")
Soy_planted$OverallANSI  <- paste0(Soy_planted$State.ANSI, Soy_planted$County.ANSI)
Soy_planted$USDAagregion <- map_agregion(Soy_planted)
  
  #apply(Soy_planted$OverallANSI, 2, function(x) {counties_key_list[which(county_to_ag_region$ï..Fips == x)]})

Soy_planted_national     <- Soy_planted %>% group_by(Year) %>% summarise(harvested = sum(Value))
Soy_planted_district     <- Soy_planted %>% group_by(.dots = c('Ag.District', 'Year')) %>% summarise(harvested = sum(Value))
Soy_planted_agregion    <- Soy_planted %>% group_by(.dots = c('Year', 'USDAagregion')) %>% summarise(produced = sum(Value))

#ggplot(Soy_planted_agregion, aes(x = Year, y = produced, colour = factor(USDAagregion))) +geom_line()


  ### b) production

Soy_production          <- read.csv('Soy production_1990_2017.csv')
Soy_production$Value[Soy_production$Value == ' (D)'] <- 0

Soy_production$Value    <- as.numeric(gsub(',', '', as.character(Soy_production$Value)))
Soy_production$Value    <- Soy_production$Value / 36.74371 ### convert to metric tonnes
Soy_production          <- Soy_production[Soy_production$Program == 'SURVEY', ]

Soy_production$County.ANSI  <- formatC(Soy_production$County.ANSI, width = 3, format = "d", flag = "0")
Soy_production$OverallANSI  <- paste0(Soy_production$State.ANSI, Soy_production$County.ANSI)
Soy_production$USDAagregion <- map_agregion(Soy_production)

Soy_production_national <- Soy_production %>% group_by(Year) %>% summarise(produced = sum(Value))
Soy_production_state    <- Soy_production %>% group_by(State) %>% summarise(produced = sum(Value))
Soy_produced_agregion   <- Soy_production %>% group_by(.dots = c('Year', 'USDAagregion')) %>% summarise(produced = sum(Value))


#ggplot(Soy_produced_agregion, aes(x = Year, y = produced, colour = factor(USDAagregion))) +geom_line()


  ### c) price - state level

Soy_price               <- read.csv('Soy_price.csv')
Soy_price$Value         <-  as.numeric(as.character(Soy_price$Value))

Soy_price_state         <-  Soy_price %>% group_by(.dots = c('State', 'Year')) %>% summarise(price = mean(Value, na.rm = TRUE))
Soy_price_national      <-  Soy_price %>% group_by(Year) %>% summarise(price = mean(Value, na.rm = TRUE))


  ### d) biodiesel      

Soy_diesel               <- Soy_diesel <- read.csv('Soy diesel.csv')
blank                    <- data.frame(matrix(nrow = 10, ncol = ncol(Soy_diesel)))
colnames(blank)          <- colnames(Soy_diesel)
Soy_diesel               <- rbind(blank, Soy_diesel)
Soy_diesel$Marketing.Year                 <- c(1990:2018)
Soy_diesel$Share.used.for.biodiesel[1:10] <- 0
Soy_diesel$Biodiesel[1:10]                <- NA


  ### e) Irrigated area

Soy_Irrigated_Area              <- read.csv('Soybean Irrigated Planted Area.csv')
Soy_Irrigated_Area$Value[Soy_Irrigated_Area$Value ==' (D)'] <- 0

Soy_Irrigated_Area$Value        <- as.numeric(as.character(Soy_Irrigated_Area$Value))
Soy_Irrigated_Area$Value[is.na(Soy_Irrigated_Area$Value ==' (D)')] <- 0

Soy_Irrigated_Area$County.ANSI  <- formatC(Soy_Irrigated_Area$County.ANSI, width = 3, format = "d", flag = "0")
Soy_Irrigated_Area$OverallANSI  <- paste0(Soy_Irrigated_Area$State.ANSI, Soy_Irrigated_Area$County.ANSI)
Soy_Irrigated_Area$USDAagregion <- map_agregion(Soy_Irrigated_Area)

Soy_Irrigated_State             <- Soy_Irrigated_Area %>% group_by(Year, State) %>% summarise(total = sum(Value))
##Soy_Irrigated_Area %>% group_by(Year) %>% summarise(total = sum(Value)) significant increase over time


  ### f) Biodiesel price vs diesel

Biodiesel_economics            <- read.csv('Biodiesel Economics.csv', skip = 3)
Biodiesel_economics            <- Biodiesel_economics[1:146, 1:3]
Biodiesel_economics$Month.yr   <- gsub('-', ' ', Biodiesel_economics$Month.yr)
Biodiesel_economics$Month.yr   <- gsub(' ', ' 20', Biodiesel_economics$Month.yr)
Biodiesel_economics$Month.yr   <- as.yearmon(Biodiesel_economics$Month.yr[1], format = '%b %Y')


  ### z) Delta (annual change by state)

###################################

### NEEDS TIDYING UP

###################################


test                     <- Soy_planted[, c('State', 'County','Ag.District', 'Year', 'Value')]
test                     <- test %>% group_by(.dots = c('County', 'Year'))
test                     <- spread(test, Year, Value)


Soy_wide                 <- spread(Soy_planted[, c('State', 'Ag.District', 'Year', 'Value')],Year, value = Value)
Soy_delta                <- Soy_wide

for (i in 5:ncol(Soy_wide)) {

Soy_delta[, i]           <- Soy_delta[, i] - Soy_delta[, i-1]

}

templist <- list()

for(i in 5:ncol(Soy_delta)) {
  
  templist[[i]] <- (aggregate(Soy_delta[, i] ~ State, data = Soy_delta, sum))
  
}

for (i in 5:length(templist)) {
  templist[[i]][, 3] <- i + 1986
}

t           <- rbind.fill(templist[5:31])
t           <- spread(t, V3, value = `Soy_delta[, i]`)
t$`1990`    <- 0
t           <- t[, c(1, 29, 3:28)]

Soy_delta_state <- t

t           <- data.frame(t(t))
n           <- (t[1, ])
colnames(t) <- n
t           <- apply(t, 2, as.numeric)

apply(t, 2, function(x) {sum(as.numeric(x[25:31]), na.rm = TRUE)})

t %>% group_by(State) %>% summarise()



##################################################################################

  ### 2) Corn

##################################################################################


setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Corn')

### USA


  ### a) planted area

Corn1_planted          <- read.csv('Corn_county_planted1.csv')
Corn2_planted          <- read.csv('Corn_county_planted2.csv')
Corn_planted           <- rbind(Corn1_planted ,Corn2_planted)

remove(Corn1_planted, Corn2_planted)

Corn_planted          <- Corn_planted[Corn_planted$Program != 'CENSUS',]
Corn_planted          <- Corn_planted[Corn_planted$Period == 'YEAR',]
Corn_planted$Value    <- as.numeric(gsub(',', '', Corn_planted$Value))

Corn_planted$County.ANSI  <- formatC(Corn_planted$County.ANSI, width = 3, format = "d", flag = "0")
Corn_planted$OverallANSI  <- paste0(Corn_planted$State.ANSI, Corn_planted$County.ANSI)
Corn_planted$USDAagregion <- map_agregion(Corn_planted)

Corn_planted_national <- Corn_planted %>% group_by(Year) %>% summarise(planted = sum(Value))
Corn_planted_state    <- Corn_planted %>% group_by(State) %>% summarise(planted = sum(Value))


  ### b) harvested area

Corn1_harvested       <- read.csv('Corn Harvested Area.csv')
Corn2_harvested       <- read.csv('Corn Harvested Area_2ndpart.csv')
Corn_harvested        <- rbind(Corn1_harvested ,Corn2_harvested)

remove(Corn1_harvested, Corn2_harvested)

Corn_harvested        <- Corn_harvested[Corn_harvested$Program != 'CENSUS', ]
Corn_harvested        <- Corn_harvested[Corn_harvested$Period == 'YEAR', ]
Corn_harvested$Value  <- as.numeric(gsub(',', '', Corn_harvested$Value))

Corn_harvested $County.ANSI  <- formatC(Corn_harvested$County.ANSI, width = 3, format = "d", flag = "0")
Corn_harvested $OverallANSI  <- paste0(Corn_harvested$State.ANSI, Corn_harvested$County.ANSI)
Corn_harvested $USDAagregion <- map_agregion(Corn_harvested)

Corn_harvested_national <- Corn_harvested %>% group_by(Year) %>% summarise(harvested = sum(Value))
Corn_harvested_state  <- Corn_harvested %>% group_by(State) %>% summarise(harvested = sum(Value))


  ### c) production

Corn1_produced        <- read.csv('Corn_produced1.csv')
Corn2_produced        <- read.csv('Corn_produced2.csv')
Corn_produced         <- rbind(Corn1_produced ,Corn2_produced)

remove(Corn1_produced , Corn2_produced)

Corn_produced         <- Corn_produced[Corn_produced$Program != 'CENSUS', ]
Corn_produced         <- Corn_produced[Corn_produced$Period == 'YEAR', ]
Corn_produced$Value   <- as.numeric(gsub(',', '', Corn_produced$Value))
Corn_produced$Value   <- Corn_produced$Value / 39.368

Corn_produced$County.ANSI  <- formatC(Corn_produced$County.ANSI, width = 3, format = "d", flag = "0")
Corn_produced$OverallANSI  <- paste0(Corn_produced$State.ANSI, Corn_produced$County.ANSI)
Corn_produced$USDAagregion <- map_agregion(Corn_produced)

Corn_produced_national <- Corn_produced %>% group_by(Year) %>% summarise(produced = sum(Value))
Corn_produced_state    <- Corn_produced %>% group_by(State) %>% summarise(produced = sum(Value))
Corn_produced_agregion <- Corn_produced %>% group_by(USDAagregion) %>% summarise(produced = sum(Value))


  ### d) price

Corn_price              <-  read.csv('Corn_price.csv')
Corn_price$Value        <-  as.numeric(as.character(Corn_price$Value))

Corn_price$County.ANSI  <- formatC(Corn_price$County.ANSI, width = 3, format = "d", flag = "0")
Corn_price$OverallANSI  <- paste0(Corn_price$State.ANSI, Corn_produced$County.ANSI)
Corn_price$USDAagregion <- map_agregion(Corn_price)

Corn_price_state        <-  Corn_price %>% group_by(.dots = c('State', 'Year')) %>% summarise(price = mean(Value, na.rm = TRUE))
Corn_price_national     <-  Corn_price %>% group_by(Year) %>% summarise(price = mean(Value, na.rm = TRUE))


  ### e) Ethanol production

Corn_ethanol           <- read.csv('table05.csv', skip = 3)
Corn_ethanol           <- Corn_ethanol[contains('MY', vars = Corn_ethanol$X), ]
Corn_ethanol$Marketing.year.and.quarter1 <- c(1981:2019)
Corn_ethanol           <- Corn_ethanol[, -c(10:14)]
Corn_ethanol$Fuel.ethanol.use <- as.numeric(as.character(gsub(',', '', Corn_ethanol$Fuel.ethanol.use)))


  ### f) Ethanol economics - key variables are Ethanol geg (gasoline eq gallons) and Gasoline

Corn_ethanol_economics           <- read.csv('Corn Ethanol economics.csv', skip = 2)
Corn_ethanol_economics           <- Corn_ethanol_economics[contains('Avg', vars = Corn_ethanol_economics$X), ]
Corn_ethanol_economics$Marketing.year.and.quarter1 <- c(1983:2018)
Corn_ethanol_economics           <- Corn_ethanol_economics[, -c(9:10)]
#Corn_ethanol_economics$subsidy   <- ifelse(Corn_ethanol_economics$Marketing.year.and.quarter1 <= 2000, 0.6, 0)
#Corn_ethanol_economics$subsidy   <- ifelse(Corn_ethanol_economics$Marketing.year.and.quarter1 <= 2008 & Corn_ethanol_economics$Marketing.year.and.quarter1 > 2000, 0.51, Corn_ethanol_economics$subsidy)
#Corn_ethanol_economics$subsidy   <- ifelse(Corn_ethanol_economics$Marketing.year.and.quarter1 <= 2011 & Corn_ethanol_economics$Marketing.year.and.quarter1 > 2008, 0.45, Corn_ethanol_economics$subsidy)


### g) Irrigated area

Corn_Irrigated_Area              <- read.csv('Corn Irrigated Area planted.csv')
Corn_Irrigated_Area$Value[Corn_Irrigated_Area$Value ==' (D)'] <- 0

Corn_Irrigated_Area$County.ANSI  <- formatC(Corn_Irrigated_Area$County.ANSI, width = 3, format = "d", flag = "0")
Corn_Irrigated_Area$OverallANSI  <- paste0(Corn_Irrigated_Area$State.ANSI, Corn_Irrigated_Area$County.ANSI)
Corn_Irrigated_Area$USDAagregion <- map_agregion(Corn_Irrigated_Area)

Corn_Irrigated_Area$Value        <- as.numeric(as.character(Corn_Irrigated_Area$Value))
Corn_Irrigated_Area$Value[is.na(Corn_Irrigated_Area$Value ==' (D)')] <- 0
Corn_Irrigated_State             <- Corn_Irrigated_Area %>% group_by(Year, State) %>% summarise(total = sum(Value))
Corn_Irrigated_Area %>% group_by(Year) %>% summarise(total = sum(Value)) ###significant increase over time



##################################################################################

### 3) Climate

##################################################################################


  ### Prec & Temp



##################################################################################

### 4) Other

##################################################################################


### USA

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other')


  ### a) CRP lands

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Agricultural context')
CRP                     <- read.csv('CRP Rental Payment History By County1.csv', skip = 3)
colnames(CRP)           <- c('State', 'County', 'ID', c(1986:2017))
CRP[,4:35]              <- apply(CRP[, 4:35], 2, function(x){as.numeric(gsub(',', '', x))})


CRP_payment             <- read.csv('CRP payment amount.csv', skip = 3)
colnames(CRP_payment)   <- c('State', 'County', 'ID', c(1986:2017))
CRP_payment[,4:35]      <- apply(CRP_payment[, 4:35], 2, function(x){as.numeric(gsub(',', '', x))})


#CRP_payment_long       <- gather(CRP_payment[4:35])
#CRP_payment_long       <- aggregate(value ~ key, data = CRP_payment_long, FUN = sum)
#CRP_payment_long$key   <- as.numeric(as.character(CRP_payment_long$key))
#ggplot(CRP_payment_long, aes(x = key, y = value)) + geom_line() #+ scale_y_continuous(limits = c(0, 40000000))

#CRP_long       <- gather(CRP[4:35])
#CRP_long       <- aggregate(value ~ key, data = CRP_long, FUN = sum)
#CRP_long$key   <- as.numeric(as.character(CRP_long$key))
#ggplot(CRP_long, aes(x = key, y = value)) + geom_line() + scale_y_continuous(limits = c(0, 40000000))

### was the amount paid in CRP reduced?


  ### b) etc

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Agricultural context')

Fertiliser_inputs        <- read.csv('Fertiliser inputs.csv')
Fertiliser_inputs$Value  <- as.numeric(gsub(',', '', Fertiliser_inputs$Value))


  ### c) Land value

Land_value              <- read.csv('Land Value.csv')
Land_value$Value        <- as.numeric(gsub(',', '', Land_value$Value))

  ### d) Yield Gap

Yield_Gap               <- read.csv('USAYieldGaps.csv')
Yield_Gap_State         <- strsplit(as.character(Yield_Gap$ï..STATIONNAME), ' ')
Yield_Gap_State         <- lapply(Yield_Gap_State, function(x) {x[2]})
Yield_Gap$State         <- unlist(Yield_Gap_State)
Yield_Gap               <- Yield_Gap[1:1620, ]
Yield_Gap$YieldGap      <- Yield_Gap$YP - Yield_Gap$YA

Yield_Gap$State[Yield_Gap$State == 'Charleston'] <- 'OH'
Yield_Gap$State[Yield_Gap$State == 'Lafayette']  <- 'IN'
Yield_Gap$State[Yield_Gap$State == 'Center']     <- 'NE'
Yield_Gap$State[Yield_Gap$State == 'City']       <- 'KS'
Yield_Gap$State[Yield_Gap$State == 'Platte']     <- 'NE'
Yield_Gap$State[Yield_Gap$State == 'Joseph']     <- 'MO'
Yield_Gap$State[Yield_Gap$State == 'Mille']      <- 'TX'

Yield_Gap %>% group_by(State) %>% summarise(YP = mean(YieldGap, na.rm = TRUE))

