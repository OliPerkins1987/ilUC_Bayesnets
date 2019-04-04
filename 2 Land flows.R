
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd('E:/Modules/Dissertation/Data/Processed Dat')

### script functions

fill_na                         <- colwise(function(x) {ifelse(is.na(x), 0, x)})


#############################################################################

### 1) Import total land stocks

#############################################################################

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

Land_stocks.State               <- filter(Land_stocks.State, State %in% c('OHIO', 'INDIANA', 'ILLINOIS', 'IOWA', 'MISSOURI', 'NORTH DAOKTA', 'SOUTH DAKOTA', 'NEBRASKA', 'KANSAS', 'MISSISSIPPI', 'ARKANSAS', 'LOUISIANA'))

CRP                             <- read.csv('CRP_Processed_agregion.csv')
CRP                             <- CRP[, 2:ncol(CRP)]

CRP.Agregion                    <- CRP %>% group_by(.dots = c('key', 'USDAagregion')) %>%
                                      summarise(CRP_stock = sum(value))



### Additional commodities

extra_commods                   <- read.csv('extra_commodities.csv')

extra_commods_counties          <- extra_commods[extra_commods$Geo.Level =='COUNTY', ]
extra_commods_states            <- extra_commods[extra_commods$Geo.Level == 'STATE', ]


#############################################################################

### 2) Import and process Soy data

#############################################################################

Soy                             <- read.csv("Processed_Soy.csv", stringsAsFactors = FALSE)
Soy                             <- Soy[, 2:ncol(Soy)]
Soy                             <- fill_na(Soy)
Soy$Yield                       <- Soy$SOYBEANS...PRODUCTION..MEASURED.IN.BU / Soy$Acres_Planted
Soy$Yield_Irrigated             <- Soy$SOYBEANS..IRRIGATED...PRODUCTION..MEASURED.IN.BU / Soy$SOYBEANS..IRRIGATED...ACRES.HARVESTED
Soy$Yield_NotIrrigated_Planted  <- Soy$Acres_Planted - Soy$SOYBEANS..IRRIGATED...ACRES.PLANTED
Soy$Yield_NotIrrigated_Produced <- Soy$SOYBEANS...PRODUCTION..MEASURED.IN.BU - Soy$SOYBEANS..IRRIGATED...PRODUCTION..MEASURED.IN.BU
Soy$Yield_NotIrrigated_Yield    <- Soy$Yield_NotIrrigated_Produced / Soy$Yield_NotIrrigated_Planted 


Soy.Agregion                    <- Soy %>% group_by(.dots = c('Year', 'USDAagregion')) %>% 
                                summarise(Production = sum(SOYBEANS...PRODUCTION..MEASURED.IN.BU), 
                                          Planted = sum(Acres_Planted), 
                                          Harvested = sum(SOYBEANS...ACRES.HARVESTED), 
                                          Half.Planted = mean(Half.Planted.Day))





#############################################################################

### 3) Import and Process Corn

#############################################################################

### Corn needs price


Corn                             <- read.csv("Processed_Corn.csv", stringsAsFactors = FALSE)
Corn                             <- Corn[, 2:ncol(Corn)]
Corn                             <- fill_na(Corn)
colnames(Corn)[15]               <- 'Half.Planted'
Corn$Yield                       <- Corn$CORN..GRAIN...PRODUCTION..MEASURED.IN.BU / Corn$Acres_Planted
Corn$Yield_Irrigated             <- Corn$CORN..GRAIN..IRRIGATED...PRODUCTION..MEASURED.IN.BU / Corn$CORN..IRRIGATED...ACRES.PLANTED
Corn$Yield_NotIrrigated_Planted  <- Corn$Acres_Planted - Corn$CORN..IRRIGATED...ACRES.PLANTED
Corn$Yield_NotIrrigated_Produced <- Corn$CORN..GRAIN...PRODUCTION..MEASURED.IN.BU - Corn$CORN..GRAIN..IRRIGATED...PRODUCTION..MEASURED.IN.BU
Corn$Yield_NotIrrigated_Yield    <- Corn$Yield_NotIrrigated_Produced / Corn$Yield_NotIrrigated_Planted 


Corn.Agregion                    <- Corn %>% group_by(.dots = c('Year', 'USDAagregion')) %>% 
  summarise(Production = sum(CORN..GRAIN...PRODUCTION..MEASURED.IN.BU), 
            Planted = sum(Acres_Planted), 
            Harvested = sum(CORN..GRAIN...ACRES.HARVESTED), 
            Half.Planted = mean(Half.Planted))



#################################################################################################################

### 4) Exploratory plots

#################################################################################################################


    ### 4a) totals

### Total cropland shows declining trend?

ggplot(Land_stocks.Agregion, aes(x = Year, y = Total, colour = factor(USDAagregion))) + geom_line()
ggplot(data.frame(filter(Land_stocks.Agregion, USDAagregion %in% c(1, 2, 3, 4))), aes(x = Year, y = Pasture, colour = factor(USDAagregion))) + geom_line()
ggplot(Land_stocks.Agregion, aes(x = Year, y = Pasture, colour = factor(USDAagregion))) + geom_line()
ggplot(data.frame(filter(Land_stocks.Agregion, USDAagregion %in% c(1, 2, 3, 4))), aes(x = Year, y = Idle, colour = factor(USDAagregion))) + geom_line()


### Pastured crop has most steep decline

ggplot(Land_stocks.Agregion, aes(x = Year, y = Pastured_crop, colour = factor(USDAagregion))) + geom_line()

### Fascinating - we have conversion of Pastured_cropland to crops across the board, which creates a static trend in footprint

ggplot(Land_stocks.Agregion, aes(x = Year, y = Cropland - Pastured_crop, colour = factor(USDAagregion))) + geom_line()

### Similar trend observed in CRP

ggplot(data.frame(filter(CRP.Agregion, USDAagregion %in% c(1, 2, 3, 4))), aes(x = key, y = CRP_stock, colour = factor(USDAagregion))) + geom_line()



### by state

ggplot(Land_stocks.State, aes(x = Year, y = Cropland, colour = State)) + geom_line()
ggplot(Land_stocks.State, aes(x = Year, y = Pasture, colour = State)) + geom_line()


    ### 4b) Soy

ggplot(Soy.Agregion, aes(x = Year, y = Planted, colour = factor(USDAagregion))) + geom_line()




    ### 4c) Corn

ggplot(Corn.Agregion, aes(x = Year, y = Production, colour = factor(USDAagregion))) + geom_line()




#################################################################################################################

### 5) Trends in Corn and Soy

#################################################################################################################

Soy.Agregion.Census    <- Soy.Agregion[Soy.Agregion$Year %in% c(1997, 2002, 2007, 2012), ]
Corn.Agregion.Census   <- Corn.Agregion[Corn.Agregion$Year %in% c(1997, 2002, 2007, 2012), ]

land.merged             <- merge(Land_stocks.Agregion, Soy.Agregion.Census, by = c('Year', 'USDAagregion'), all.x = TRUE)
land.merged             <- merge(land.merged, Corn.Agregion.Census, by = c('Year', 'USDAagregion'), all.x = TRUE)
land.merged             <- fill_na(land.merged)

land.merged$Soy_pct     <- land.merged$Planted.x / land.merged$Cropland
land.merged$Corn_pct    <- land.merged$Planted.y  / land.merged$Cropland


ggplot(land.merged, aes(x = Year, y = Soy_pct, colour = factor(USDAagregion))) + geom_line()
ggplot(land.merged, aes(x = Year, y = Corn_pct, colour = factor(USDAagregion))) + geom_line()


#################################################################################################################

### 6) Create linear models for in between census years based on Corn / Soy production

#################################################################################################################


Land_use.models <- list()


for (i in 1:length(unique(land.merged$USDAagregion))) {
  
  Land_use.models[[i]] <- lm(Cropland - Pastured_crop ~ Planted.x + Planted.y, data = land.merged, subset = land.merged$USDAagregion == unique(land.merged$USDAagregion)[i])    
  
}


predict.frame          <- merge(Soy.Agregion, Land_stocks.Agregion, by = c('Year', 'USDAagregion'), all.x = TRUE)
predict.frame          <- merge(predict.frame, Corn.Agregion, by = c('Year', 'USDAagregion'), all.x = TRUE)
preds                  <- list()


for (i in 1:length(Land_use.models)) {
  
  preds[[i]] <- predict.lm(Land_use.models[[i]], predict.frame[predict.frame$USDAagregion == unique(predict.frame$USDAagregion)[i], ])
  
}


results               <- data.frame(preds[2:9])
rownames(results)     <- c(1990:2017)
colnames(results)     <- c(1:7, 9)
results               <- gather(results)
results$Year          <- c(1990:2017)

ggplot(results, aes(x = Year, y = value, colour = key)) + geom_line()

results.census        <- results[results$Year %in% c(1997, 2002, 2007, 2012), ]

####### This needs validating to see how accurate it is

####### Models seems to be gunk - need to use a hierachical model / drill down to county level for key regions



#################################################################################################################

### 7) Commodity Flows

#################################################################################################################

library(lmtest)

granger_res   <- list()

for(i in c(1, 2, 4:9)) {

granger_res[[i]]   <- grangertest(Corn.Agregion$Planted[Soy.Agregion$USDAagregion == 3], Soy.Agregion$Planted[Soy.Agregion$USDAagregion == i], order = 3)

}

### we find that Soy flows in heartland gcause soy flows in regions 5 & 6

library(bnlearn)



