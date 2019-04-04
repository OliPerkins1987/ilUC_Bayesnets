


library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)


setwd('E:/Modules/Dissertation/Data/Processed Dat')


#############################################################################

### 1) Script functions

#############################################################################


### Delta by group

Delta_by_group                  <- function(x, y) {
  
  t <- list()
  
  for (i in 1:length(levels(factor(x$USDAagregion)))) {
    
    t[[i]] <- data.frame(filter(x, USDAagregion == levels(factor(x$USDAagregion))[i]))
    
  }
  
  s        <- lapply(t, function(x) {diff(x[, y])})
  s        <- lapply(s, function(x) {data.frame(c(0, x))})
  
  ### Combine frames
  
  s                          <- rbind.fill(s)
  r                          <- rbind.fill(t)
  try(r[, ncol(r)+1]         <- s)
  colnames(r)[ncol(r)]       <- paste0('Delta_', y)
  
  return(r)
  
}


### Delta by State

Delta_by_State                  <- function(x, y) {
  
  t <- list()
  
  for (i in 1:length(levels(factor(x$State)))) {
    
    t[[i]] <- data.frame(filter(x, State == levels(factor(x$State))[i]))
    
  }
  
  s        <- lapply(t, function(x) {diff(x[, y])})
  s        <- lapply(s, function(x) {data.frame(c(0, x))})
  
  ### Combine frames
  
  s                          <- rbind.fill(s)
  r                          <- rbind.fill(t)
  try(r[, ncol(r)+1]         <- s)
  colnames(r)[ncol(r)]       <- paste0('Delta_', y)
  
  return(r)
  
}



### fillna

fill_na                      <- colwise(function(x) {ifelse(is.na(x), 0, x)})

### unfillna

unfill_na                    <- colwise(function(x) ifelse(x == 0, NA, x))

### no negs

no_negs                      <- colwise(function(x) ifelse(x < 0, 0, x))


#############################################################################

### 2) Import and process Soy data

#############################################################################


Soy                             <- read.csv("Processed_Soy.csv", stringsAsFactors = FALSE)
Soy                             <- Soy[, 2:ncol(Soy)]
Soy                             <- fill_na(Soy)
Soy$Yield                       <- Soy$SOYBEANS...PRODUCTION..MEASURED.IN.BU / Soy$SOYBEANS...ACRES.HARVESTED
Soy$Yield_Irrigated             <- Soy$SOYBEANS..IRRIGATED...PRODUCTION..MEASURED.IN.BU / Soy$SOYBEANS..IRRIGATED...ACRES.HARVESTED
Soy$NotIrrigated_Planted        <- Soy$Acres_Planted - Soy$SOYBEANS..IRRIGATED...ACRES.PLANTED
Soy$NotIrrigated_Produced       <- Soy$SOYBEANS...PRODUCTION..MEASURED.IN.BU - Soy$SOYBEANS..IRRIGATED...PRODUCTION..MEASURED.IN.BU
Soy$NotIrrigated_Yield          <- Soy$NotIrrigated_Produced / Soy$NotIrrigated_Planted 

Soy <- Soy[!is.infinite(rowSums(Soy[, 12:23])),]
Soy <- Soy[Soy$County != 'OTHER (COMBINED) COUNTIES', ]

Soy                            <- unfill_na(Soy)

Soy.Agregion                    <- Soy %>% group_by(.dots = c('Year', 'USDAagregion')) %>% 
  summarise(Production = sum(SOYBEANS...PRODUCTION..MEASURED.IN.BU, na.rm = TRUE), 
            Planted = sum(Acres_Planted, na.rm = TRUE), 
            Harvested = sum(SOYBEANS...ACRES.HARVESTED, na.rm = TRUE), 
            Half.Planted = mean(Half.Planted.Day, na.rm = TRUE),
            Yield.Rain = mean(NotIrrigated_Yield, na.rm = TRUE), 
            Irrigated.Yield = mean(Yield_Irrigated, na.rm = TRUE), 
            Irrigated.Planted = sum(SOYBEANS..IRRIGATED...ACRES.PLANTED, na.rm = TRUE), 
            Yield = mean(Yield, na.rm = TRUE))

Soy.Total                   <- Soy %>% group_by(Year) %>% 
  summarise(Production = sum(SOYBEANS...PRODUCTION..MEASURED.IN.BU, na.rm = TRUE), 
            Planted = sum(Acres_Planted, na.rm = TRUE), 
            Harvested = sum(SOYBEANS...ACRES.HARVESTED, na.rm = TRUE), 
            Half.Planted = mean(Half.Planted.Day, na.rm = TRUE),
            Yield.Rain = mean(NotIrrigated_Yield, na.rm = TRUE), 
            Irrigated.Yield = mean(Yield_Irrigated, na.rm = TRUE), 
            Irrigated.Planted = sum(SOYBEANS..IRRIGATED...ACRES.PLANTED, na.rm = TRUE), 
            Yield = mean(Yield, na.rm = TRUE))




#############################################################################

### 3) Import and process corn data

#############################################################################


Corn                             <- read.csv("Processed_Corn_Counties.csv", stringsAsFactors = FALSE)
Corn                             <- Corn[, 2:ncol(Corn)]
Corn                             <- fill_na(Corn)
Corn$CORN..GRAIN...PRODUCTION..MEASURED.IN.BU  <- Corn$CORN..GRAIN...PRODUCTION..MEASURED.IN.BU * 39.368
Corn$Yield                       <- Corn$CORN..GRAIN...PRODUCTION..MEASURED.IN.BU / Corn$CORN..GRAIN...ACRES.HARVESTED
Corn$Yield_Irrigated             <- Corn$CORN..GRAIN..IRRIGATED...PRODUCTION..MEASURED.IN.BU / Corn$CORN..GRAIN..IRRIGATED...ACRES.HARVESTED
Corn                             <- fill_na(Corn)
Corn$NotIrrigated_Planted        <- Corn$Acres_Planted - Corn$CORN..IRRIGATED...ACRES.PLANTED
Corn$NotIrrigated_Produced       <- Corn$CORN..GRAIN...PRODUCTION..MEASURED.IN.BU - Corn$CORN..GRAIN..IRRIGATED...PRODUCTION..MEASURED.IN.BU 
Corn$NotIrrigated_Yield          <- Corn$NotIrrigated_Produced / Corn$NotIrrigated_Planted 

Corn                             <- Corn[Corn$County != 'OTHER (COMBINED) COUNTIES', ]
Corn                             <- unfill_na(Corn)
Corn                             <- no_negs(Corn)

Corn$NotIrrigated_Produced[Corn$NotIrrigated_Produced <1.2016]       <- 0
Corn$NotIrrigated_Yield[Corn$NotIrrigated_Produced <1.2016]          <- 0


Corn.Agregion                    <- Corn %>% group_by(.dots = c('Year', 'USDAagregion')) %>% 
  summarise(Production = sum(CORN..GRAIN...PRODUCTION..MEASURED.IN.BU, na.rm = TRUE), 
            Planted = sum(Acres_Planted, na.rm = TRUE), 
            Harvested = sum(CORN..GRAIN...ACRES.HARVESTED, na.rm = TRUE), 
            Half.Planted = mean(value, na.rm = TRUE),
            Yield.Rain = mean(NotIrrigated_Yield, na.rm = TRUE), 
            Irrigated.Yield = mean(Yield_Irrigated, na.rm = TRUE), 
            Irrigated.Planted = sum(CORN..IRRIGATED...ACRES.PLANTED, na.rm = TRUE), 
            Yield = mean(Yield, na.rm = TRUE),
            NotIrrigated_Produced = mean(NotIrrigated_Produced, na.rm = TRUE))

temp                       <- as.numeric(Corn.Agregion$Year)
Corn.Agregion              <- fill_na(Corn.Agregion)
Corn.Agregion$Year         <- temp


ggplot(Corn.Agregion, aes(x = Year, y = Irrigated.Yield, colour = factor(USDAagregion))) + geom_line()
ggplot(data.frame(filter(Corn.Agregion, USDAagregion %in% c(1,2,3,4,5))), aes(x = Year, y = Yield, colour = factor(USDAagregion))) + geom_line()
ggplot(Corn.Agregion, aes(x = Year, y = Yield.Rain, colour = factor(USDAagregion))) + geom_line()

Corn.Total                    <- Corn %>% group_by(Year) %>% 
  summarise(Production = sum(CORN..GRAIN...PRODUCTION..MEASURED.IN.BU, na.rm = TRUE), 
            Planted = sum(Acres_Planted, na.rm = TRUE), 
            Harvested = sum(CORN..GRAIN...ACRES.HARVESTED, na.rm = TRUE), 
            Half.Planted = mean(value, na.rm = TRUE),
            Yield.Rain = mean(NotIrrigated_Yield, na.rm = TRUE), 
            Irrigated.Yield = mean(Yield_Irrigated, na.rm = TRUE), 
            Irrigated.Planted = sum(CORN..IRRIGATED...ACRES.PLANTED, na.rm = TRUE), 
            Yield = mean(Yield, na.rm = TRUE),
            NotIrrigated_Produced = mean(NotIrrigated_Produced, na.rm = TRUE))



#############################################################################

### 4) Extra commodities

### Doing this over the whole period seems to be useful for quantifying flows but not displacements

### Next to try just heartland states with 5 year bins

### Need a more detailed understanding of intense in other commods and distribution of change


#############################################################################

  ### a) read data 

extra_commods       <- read.csv('extra_commodities.csv')
extra_commods.State <- extra_commods %>% group_by(.dots = c('State', 'Year', 'Data.Item')) %>% summarise(Planted = sum(Value))
extra_commods.State <- data.frame(filter(extra_commods.State, State != 'US TOTAL'))

extra_commods.wide  <- spread(extra_commods.State, 'Data.Item', 'Planted')

### for comparison

Soy.Planted.State   <- Soy %>% group_by(.dots = c('State', 'Year')) %>% summarise(Soy.Planted = sum(Acres_Planted, na.rm = TRUE))
Corn.Planted.State  <- Corn %>% group_by(.dots = c('State', 'Year'))%>% summarise(Corn.Planted = sum(Acres_Planted, na.rm = TRUE))


  ### b) merge for compairson

commods.flows        <- merge(extra_commods.wide, Soy.Planted.State, by = c('State', 'Year'), all.x = TRUE)
commods.flows        <- merge(commods.flows, Corn.Planted.State, by = c('State', 'Year'), all.x = TRUE)

commods.flows$period <- ifelse(commods.flows$Year <= 1997, 1,
                               ifelse(commods.flows$Year > 1997 & commods.flows$Year <= 2002, 2,
                                      ifelse(commods.flows$Year > 2002 & commods.flows$Year <= 2007, 3,
                                            ifelse(commods.flows$Year > 2007 & commods.flows$Year <= 2012, 
                                                   4, 5))))
                                                   

  ### c) calculate delta

for (i in 1:length(colnames(commods.flows))) {
  
  commods.flows   <- Delta_by_State(commods.flows, colnames(commods.flows)[i])
}


commods.flows     <- commods.flows[, -c(11:12, 20)]
commods.flows     <- data.frame(filter(commods.flows, Year != 1990))


  ### d) can we detect flows?

Soy.flows           <- list()
commods.flows.State <- list()

for(i in 1:length(unique(commods.flows$State))){
  
  commods.flows.State[[i]] <- data.frame(filter(commods.flows, State == unique(commods.flows$State)[i]))
  
}


for(j in 1:length(commods.flows.State)) {
  
  Soy.flows[[j]] <- 1
  
  for(i in 1:5) {
  
    try(
    
    Soy.flows[[j]] <- c(Soy.flows[[j]], cor.test(commods.flows.State[[j]][, i+9], commods.flows.State[[j]]$Soy.Planted))
  
    )
  }
}


Soy.flows          <- lapply(Soy.flows, function(x) {x[c(4:5, 13:14, 22:23, 31:32)]})


########################################################################################
#   
#   SOY BEAN FLOWS:
#
#   1) Mississippi has increasing Soy and p = 0.08 neg cor with wheat
#   2) Missouri has 6fig increasing Soy and both positive and negative correlations
#   3) No Soy in Montana
#   4) Nebraska has large increases in several commodities, but significant decrease in SORGHUM; but no correlations - Pasture?
#   5) North Carolina has a modest increase in Soy, major decrease in Corn, and explosion in Cotton
#   6) North Dakota has vast increases in Soy and Corn & sugarbeet, but no correlations - pasture?
#   7) Soy has replaced corn in Ohio, but there is no correlation - this suggests there are time lags etc.
#   8) Oaklahoma has weak increases across the board, but a strong increase in Cotton
#   9) Soy and wheat have displaced corn in Pennsylvania
#   10) South Carolina has a decrease in Soy and Corn, but an increase in Cotton and Wheat
#   11) North Dakota has a vast increase in Soybean area, and a large increase in corn and wheat (>1m)
#   12) Tennessee has modest increases in Soy and corn, but large increases in cotton and wheat
#   13) Texas has an explosion of Cotton, large decrease in Sorghum, modest changes elsewhere
#   14) Cotton and Wheat replacing Soy and Corn in Virginia (modest)
#   15) 1m Soy increase in Wisconsin, with 500k increase in Wheat
#   16) Minnesota has 3m+ increase in Soy, 1m increase in Corn, Wheat, Sugarbeet
#   17) Michigan has a 1m increase in Soy, 6 figs in Wheat and sugarbeet
#   18) Illinois has a 1m increase in Soy, 6fig increase in Maize, and modest increase elsewhere
#   19) Kentucky has a 6fig increase in Soy, modest Maize decrease, large increase in wheat
#
#
#
########################################################################################

unique(commods.flows$State)[10]

ggplot(commods.flows.State[[30]], aes(x = Year, y = Delta_Soy.Planted)) + geom_line()
ggplot(commods.flows.State[[29]], aes(x = Year, y = Delta_Corn.Planted)) + geom_line()
ggplot(commods.flows.State[[29]], aes(x = Year, y = Delta_RICE...ACRES.PLANTED)) + geom_line()
ggplot(commods.flows.State[[29]], aes(x = Year, y = Delta_COTTON...ACRES.PLANTED)) + geom_line()
ggplot(commods.flows.State[[29]], aes(x = Year, y = Delta_WHEAT...ACRES.PLANTED)) + geom_line()
ggplot(commods.flows.State[[29]], aes(x = Year, y = Delta_SORGHUM...ACRES.PLANTED)) + geom_line()
ggplot(commods.flows.State[[29]], aes(x = Year, y = Delta_SUGARBEETS...ACRES.PLANTED)) + geom_line()

sum(commods.flows.State[[14]]$Delta_Soy.Planted, na.rm = TRUE)
sum(commods.flows.State[[14]]$Delta_Corn.Planted, na.rm = TRUE)
sum(commods.flows.State[[14]]$Delta_RICE...ACRES.PLANTED, na.rm = TRUE)
sum(commods.flows.State[[14]]$Delta_COTTON...ACRES.PLANTED, na.rm = TRUE)
sum(commods.flows.State[[14]]$Delta_WHEAT...ACRES.PLANTED, na.rm = TRUE)
sum(commods.flows.State[[14]]$Delta_SORGHUM...ACRES.PLANTED, na.rm = TRUE)
sum(commods.flows.State[[14]]$Delta_SUGARBEETS...ACRES.PLANTED, na.rm = TRUE)

cor.test(commods.flows.State[[35]]$Delta_Soy.Planted, commods.flows.State[[35]]$Delta_Corn.Planted)


####################################################################################################

### 5) land flows by key region by time bloc

### Clear evidence of Soy vs Corn displacement in the heartland

### Some evidence of this in the Northern areas, but it's more a question of extensification

### Increased Maize, Cotton, in the Mississippi Delta, but decreased Soy, Rice, Wheat

####################################################################################################


Heartland.flows <- data.frame(filter(commods.flows, State %in% c('OHIO', 'IOWA', 
                                                                 'ILLINOIS',  
                                                                 'INDIANA', 'MISSOURI')))

Northern.flows <- data.frame(filter(commods.flows, State %in% c('MINNESOTA', 'WISCONSIN', 
                                                                 'MICHIGAN', 'NORTH DAKOTA', 'SOUTH DAKOTA',
                                                                'NEBRASKA')))

### MISSISSIPPI DELTA

Delta.flows  <- data.frame(filter(commods.flows, State %in% c('MISSISSIPPI', 'ARKANSAS', 'LOUISIANA')))

Prarie.flows <- data.frame(filter(commods.flows, State %in% c('KANSAS', 'NEBRASKA', 'OAKLAHOMA')))

Heartland.flows$period <- factor(Heartland.flows$period)

Heartland.flows.stages <- Heartland.flows %>% group_by(.dots = c('period', 'State')) %>% 
  summarise(Maize = sum(Delta_Corn.Planted, na.rm = TRUE), 
    Soybean = sum(Delta_Soy.Planted, na.rm = TRUE),
      Cotton = sum(Delta_COTTON...ACRES.PLANTED, na.rm = TRUE),
        Rice = sum(Delta_RICE...ACRES.PLANTED, na.rm = TRUE),
          Wheat = sum(Delta_WHEAT...ACRES.PLANTED, na.rm = TRUE),
            Sugarbeets = sum(Delta_SUGARBEETS...ACRES.PLANTED, na.rm = TRUE),
              Sorghum = sum(Delta_SORGHUM...ACRES.PLANTED, na.rm = TRUE))


Northern.flows.stages <- Northern.flows %>% group_by(.dots = c('period', 'State')) %>% 
  summarise(Maize = sum(Delta_Corn.Planted, na.rm = TRUE), 
            Soybean = sum(Delta_Soy.Planted, na.rm = TRUE),
            Cotton = sum(Delta_COTTON...ACRES.PLANTED, na.rm = TRUE),
            Rice = sum(Delta_RICE...ACRES.PLANTED, na.rm = TRUE),
            Wheat = sum(Delta_WHEAT...ACRES.PLANTED, na.rm = TRUE),
            Sugarbeets = sum(Delta_SUGARBEETS...ACRES.PLANTED, na.rm = TRUE),
            Sorghum = sum(Delta_SORGHUM...ACRES.PLANTED, na.rm = TRUE))

Delta.flows.stages <- Delta.flows %>% group_by(.dots = c('period', 'State')) %>% 
  summarise(Maize = sum(Delta_Corn.Planted, na.rm = TRUE), 
            Soybean = sum(Delta_Soy.Planted, na.rm = TRUE),
            Cotton = sum(Delta_COTTON...ACRES.PLANTED, na.rm = TRUE),
            Rice = sum(Delta_RICE...ACRES.PLANTED, na.rm = TRUE),
            Wheat = sum(Delta_WHEAT...ACRES.PLANTED, na.rm = TRUE),
            Sugarbeets = sum(Delta_SUGARBEETS...ACRES.PLANTED, na.rm = TRUE),
            Sorghum = sum(Delta_SORGHUM...ACRES.PLANTED, na.rm = TRUE))

Prarie.flows.stages <- Prarie.flows %>% group_by(.dots = c('period', 'State')) %>% 
  summarise(Maize = sum(Delta_Corn.Planted, na.rm = TRUE), 
            Soybean = sum(Delta_Soy.Planted, na.rm = TRUE),
            Cotton = sum(Delta_COTTON...ACRES.PLANTED, na.rm = TRUE),
            Rice = sum(Delta_RICE...ACRES.PLANTED, na.rm = TRUE),
            Wheat = sum(Delta_WHEAT...ACRES.PLANTED, na.rm = TRUE),
            Sugarbeets = sum(Delta_SUGARBEETS...ACRES.PLANTED, na.rm = TRUE),
            Sorghum = sum(Delta_SORGHUM...ACRES.PLANTED, na.rm = TRUE))

### Trends in land flows

ggplot(Prarie.flows.stages, aes(x = as.numeric(period), y = Soybean, colour = State)) + geom_line()    
ggplot(Prarie.flows.stages, aes(x = as.numeric(period), y = Maize, colour = State)) + geom_line()    
ggplot(Prarie.flows.stages, aes(x = as.numeric(period), y = Cotton, colour = State)) + geom_line()      
ggplot(Prarie.flows.stages, aes(x = as.numeric(period), y = Wheat, colour = State)) + geom_line() 
ggplot(Prarie.flows.stages, aes(x = as.numeric(period), y = Sugarbeets, colour = State)) + geom_line() 
ggplot(Prarie.flows.stages, aes(x = as.numeric(period), y = Sorghum, colour = State)) + geom_line() 

### What are the land flows in the Heartland?

cor.test(Heartland.flows.stages$Soybean, Heartland.flows.stages$Maize) ### awesome and v interesting
apply(Heartland.flows.stages[Heartland.flows.stages$period %in% c(2:4), 3:9], 2, sum, na.rm = TRUE)

cor.test(Northern.flows.stages$Soybean, Northern.flows.stages$Maize) ### awesome and v interesting
apply(Northern.flows.stages[Northern.flows.stages$period %in% c(2:4), 3:9], 2, sum, na.rm = TRUE)

cor.test(Delta.flows.stages$Soybean, Delta.flows.stages$Maize) ### awesome and v interesting
apply(Delta.flows.stages[Delta.flows.stages$period %in% c(2:4), 3:9], 2, sum, na.rm = TRUE)

cor.test(Prarie.flows.stages$Cotton, Prarie.flows.stages$Sorghum) ### awesome and v interesting
apply(Prarie.flows.stages[Prarie.flows.stages$period %in% c(2:4), 3:9], 2, sum, na.rm = TRUE)



##################################################################################################

### 6) Can any expansion be explained by CRP and fallowed lands?

##################################################################################################

### Read in Land stocks data

setwd('E:/Modules/Dissertation/Data/Processed Dat')

Land_stocks                     <- read.csv('Land_stocks_Processed.csv', stringsAsFactors = FALSE)
Land_stocks                     <- Land_stocks[, 2:ncol(Land_stocks)]
Land_stocks                     <- fill_na(Land_stocks)
Land_stocks$USDAagregion        <- sapply(Land_stocks$USDAagregion, function(x) {ifelse(as.numeric(x) > 10, x[-1], x)})

Land_stocks.Agregion            <- Land_stocks %>% group_by(.dots = c('Year', 'USDAagregion')) %>% 
  summarise(Idle = sum(AG.LAND..CROPLAND...EXCL.HARVESTED...PASTURED...IDLE...ACRES),
            Pastured_crop = sum(AG.LAND..CROPLAND..PASTURED.ONLY...ACRES),                    
            Cropland = sum(AG.LAND..CROPLAND...ACRES),                                   
            Pasture  = sum(AG.LAND..PASTURELAND...ACRES))

Land_stocks.Agregion$Total      <- Land_stocks.Agregion$Pastured_crop + Land_stocks.Agregion$Cropland + Land_stocks.Agregion$Pasture

Land_stocks.State               <- Land_stocks %>% group_by(.dots = c('Year', 'State')) %>% 
  summarise(Idle = sum(AG.LAND..CROPLAND...EXCL.HARVESTED...PASTURED...IDLE...ACRES),
            Pastured_crop = sum(AG.LAND..CROPLAND..PASTURED.ONLY...ACRES),                    
            Cropland = sum(AG.LAND..CROPLAND...ACRES),                                   
            Pasture  = sum(AG.LAND..PASTURELAND...ACRES))


for(i in 3:7) {

Land_stocks.State          <- Delta_by_State(Land_stocks.State, colnames(Land_stocks.State)[i])

}

Land_stocks.Heartland      <- filter(Land_stocks.State , State %in% c('OHIO', 'IOWA', 
                                                                         'ILLINOIS',  
                                                                         'INDIANA', 'MISSOURI'))


Land_stocks.North          <- filter(Land_stocks.State, State %in% c('MINNESOTA', 'WISCONSIN', 
                                                                         'MICHIGAN', 'NORTH DAKOTA', 'SOUTH DAKOTA',
                                                                         'NEBRASKA'))


Land_stocks.Prarie         <- filter(Land_stocks.State, State %in% c('KANSAS', 'NEBRASKA', 'OAKLAHOMA'))

Land_stocks.Delta          <- filter(Land_stocks.State, State %in% c('MISSISSIPPI', 'ARKANSAS', 'LOUISIANA'))

apply(Land_stocks.Delta[, c(7:10)], 2, sum, na.rm = TRUE)

  ### CRP lands

CRP                             <- read.csv('CRP_Processed_agregion.csv')
CRP                             <- CRP[, 2:ncol(CRP)]
CRP                             <- data.frame(filter(CRP, key >=1992))

CRP.Agregion                    <- CRP %>% group_by(.dots = c('USDAagregion', 'key')) %>%
  summarise(CRP_stock = sum(value))

CRP.Agregion                    <- Delta_by_group(CRP.Agregion, 'CRP_stock')

sum(filter(CRP.Agregion, USDAagregion == 4)$Delta_CRP_stock, na.rm = TRUE)




##################################################################################################

### 7) Granger Causality between land flows 

##################################################################################################









