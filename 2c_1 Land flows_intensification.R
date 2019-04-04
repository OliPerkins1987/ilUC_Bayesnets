

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd('E:/Modules/Dissertation/Data/Processed Dat')


  ### Function to determine delta by group


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


  ### fillna

fill_na                      <- colwise(function(x) {ifelse(is.na(x), 0, x)})

  ### unfillna

unfill_na                    <- colwise(function(x) ifelse(x == 0, NA, x))

  ### no negs

no_negs                      <- colwise(function(x) ifelse(x < 0, 0, x))


  ### Data to correct for inflation


Inflation.correct                     <- data.frame(inflation = c(1.92, 1.84, 1.79, 1.74,1.69, 1.65, 1.60, 1.56,
                                                                  1.54, 1.51, 1.46, 1.42, 1.40, 1.36, 1.33, 1.29, 
                                                                  1.25, 1.21, 1.17, 1.17, 1.15, 1.12,1.09, 1.08, 1.06, 
                                                                  1.06,1.05, 1.02, 1), Year = c(1990:2018))

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

### 3) Yield trends for Soy

#############################################################################


  ### Make Delta frames

Soy.Agregion                      <- Delta_by_group(Soy.Agregion, 'Yield.Rain')
Soy.Agregion                      <- Delta_by_group(Soy.Agregion, 'Irrigated.Yield')
Soy.Agregion                      <- Delta_by_group(Soy.Agregion, 'Yield')
Soy.Agregion$ExpectedYield_High   <- (32.1 / 27.216) * 2.471 ## kg ha-1 to bushels per acre
Soy.Agregion$ExpectedYield_Low    <- (20.8 / 27.216) * 2.471
Soy.Agregion$ExpectedYield_Medium <- (26.45 / 27.216) * 2.471

Soy.Agregion.Delta                <- Soy.Agregion[Soy.Agregion$Year != 1990, ]

Soy.Agregion.Delta %>% group_by(factor(USDAagregion)) %>% summarise(Yield.Growth = mean(Yield, na.rm = TRUE))


  ### analyse

teg <- filter(Soy.Agregion.Delta, !is.infinite(Delta_Yield))
teg <- filter(teg, !is.na(Delta_Yield))
teg <- filter(teg, Delta_Yield != 0)
teg <- teg[-c(72, 73, 107, 108), ]

mean(teg$Delta_Yield, na.rm = TRUE)

ggplot(teg, aes(x = Year, y = Yield, colour = factor(USDAagregion))) + geom_line()


#############################################################################

### 4) Import and process corn data

#############################################################################


Corn                             <- read.csv("Processed_Corn.csv", stringsAsFactors = FALSE)
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



  ### 3b) Yield Potential

Delta_by_station                 <- function(x, y) {
  
  t <- list()
  
  for (i in 1:length(levels(factor(x$ï..STATIONNAME)))) {
    
    t[[i]] <- data.frame(filter(x, ï..STATIONNAME == levels(factor(x$ï..STATIONNAME))[i]))
    
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



Yield_Gap                <- read.csv('Yield_Gap.csv')
Yield_Gap[, c(4:9, 13)]  <- apply(Yield_Gap[, c(4:9, 13)], 2, as.numeric)
Yield_Gap                <- Delta_by_station(Yield_Gap, 'YP')
Yield_Gap                <- Yield_Gap[Yield_Gap$Delta_YP != 0, ]

Yield_Gap_State          <- Yield_Gap %>% group_by(.dots = c('HARVESTYEAR', 'State')) %>% summarise(YP = mean(YP, na.rm = TRUE))
Yield_Gap_Year           <- Yield_Gap %>% group_by(HARVESTYEAR) %>% summarise(Delta_YP = mean(Delta_YP, na.rm = TRUE))

Yield_Actual_State          <- Corn %>% group_by(.dots = c('Year', 'State')) %>% summarise(YA = mean(NotIrrigated_Yield, na.rm = TRUE))
Yield_Actual_Year           <- Yield_Gap %>% group_by(HARVESTYEAR) %>% summarise(YA = mean(YA, na.rm = TRUE))



#############################################################################

### 4) Yield trends for Corn

#############################################################################


Corn.Agregion                      <- Delta_by_group(Corn.Agregion, 'Yield.Rain')
Corn.Agregion                      <- Delta_by_group(Corn.Agregion, 'Irrigated.Yield')
Corn.Agregion                      <- Delta_by_group(Corn.Agregion, 'Yield')

ggplot(Corn.Agregion, aes(x = Year, y = Delta_Irrigated.Yield, colour = factor(USDAagregion))) + geom_line()
ggplot(Corn.Agregion, aes(x = Year, y = Delta_Yield, colour = factor(USDAagregion))) + geom_line()
ggplot(Corn.Agregion, aes(x = Year, y = Delta_Yield.Rain, colour = factor(USDAagregion))) + geom_line()

sum(Corn.Agregion$Delta_Irrigated.Yield)
sum(Corn.Agregion$Delta_Yield)
sum(Corn.Agregion$Delta_Yield.Rain[!is.infinite((Corn.Agregion$Delta_Yield.Rain))], na.rm = TRUE)


####### THEREFORE - YIELDS ARE INCREASING ACROSS THE BOARD IN CORN, WITH NO DIFF BETWEEN IRRIG AND RAIN 


ggplot(data.frame(filter(Yield_Gap_State, State %in% c('OH', 'ND', 'SD', 'IL', 'IO'))), 
       aes(x = HARVESTYEAR, y = YP, colour = State)) + geom_line()

teg <- Yield_Gap_State %>% group_by(State) %>% summarise(total = sum(Delta_YP))
sum(teg$total)

ggplot(Yield_Gap_Year, aes(x = HARVESTYEAR, y = Delta_YP)) + geom_line()

ggplot(data.frame(filter(Yield_Actual_State, State %in% c('OHIO', 'NORTH DAKOTA', 'SOUTH DAKOTA', 'ILLINOIS', 'IOWA'))), aes(x = Year, y = YA, colour = State)) + geom_line()


####### HOWEVER, THERE IS ALMOST NO INCREASE IN YIELD POTENTIAL ACROSS THESE YEARS BY STATE


#############################################################################

### 5) Intensification Metrics

#############################################################################


### 5.1) Annual National Data

### This data is an annual total from USDA 

Annual.fertiliser.input <- structure(list(Year = 1991:2017, total = c(15879200000, 14858790000, 
                                                                      14562060000, 15463500000, 1.65e+10, 1.744e+10, 1.7004e+10, 1.6324e+10, 
                                                                      1.4949e+10, 2.92e+10, 2.9252e+10, 2.688e+10, 18183200000, 20242600000, 
                                                                      22097700000, 22262500000, 28761700000, 35661600000, 31367700000, 
                                                                      32211500000, 3.7296e+10, 42052200000, 40759200000, 39718200000, 
                                                                      36326200000, 3.3096e+10, 29916600000)), class = c("tbl_df", "tbl", 
            
                                                                                                                                                                                                            "data.frame"), row.names = c(NA, -27L))
#teg <- Corn %>% group_by(Year) %>% summarise(Yield = mean(Yield, na.rm = TRUE))
Annual.fertiliser.input$Yield <- teg$Yield[2:28]
plot(Annual.fertiliser.input$total, Annual.fertiliser.input$Yield)


### 5.2) Fertilisation by Region / State

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Intensification')

Fertiliser_Expense_Region         <- read.csv('Fertiliser_Expense_Region.csv')
Fertiliser_Expense_Region$Value   <- as.numeric(gsub(',', '', Fertiliser_Expense_Region$Value))
Fertiliser_Expense_Region$Value   <- apply(Fertiliser_Expense_Region, 1, function(x) {as.numeric(x[20]) * Inflation.correct$inflation[which(Inflation.correct$Year == x[2])]})

teg <- Fertiliser_Expense_Region %>% group_by(Region) %>% summarise(Years = mean(Year, na.rm = TRUE))
teg <- filter(teg, Years == 1996.5)

Fertiliser_Expense_Region_Early   <- filter(Fertiliser_Expense_Region, Region %in% teg$Region)

Fertiliser_Expense_State          <- read.csv('Fertiliser_Expense_State.csv')
Fertiliser_Expense_State$Value    <- as.numeric(gsub(',', '', Fertiliser_Expense_State$Value))
Fertiliser_Expense_State$Value    <- apply(Fertiliser_Expense_State, 1, function(x) {as.numeric(x[20]) * Inflation.correct$inflation[which(Inflation.correct$Year == x[2])]})







#############################################################################

### 6) Analysis of Intensification Drivers

#############################################################################


### What is the relationship between Irrigation and Intensification?

### Corn

Corn.Agregion.Na           <- unfill_na(Corn.Agregion)
Corn.Total <- Corn.Agregion.Na %>% group_by(Year) %>% summarise(Yield = mean(Yield, na.rm = TRUE), 
                                                                Yield.Rain = mean(Yield.Rain, na.rm = TRUE), 
                                                                Irrigated.Yield = mean(Irrigated.Yield, na.rm = TRUE),
                                                                Irrigated.Proportion = mean(Irrigated.Planted, na.rm = TRUE) / mean(Planted, na.rm = TRUE))
Corn.Total <- gather(Corn.Total[, 2:5])
Corn.Total$Year <- c(1990:2017)

ggplot(Corn.Total, aes(x = Year, y = value, colour = key)) + geom_line()

### Soy

Soy.Total <- gather(Soy.Total[, 2:8])
Soy.Total$Year <- c(1990:2017)

ggplot(Soy.Total, aes(x = Year, y = value, colour = key)) + geom_line()




#############################################################################

### 7) Analysis by subregion

#############################################################################

### Function to determine delta by state


Delta_by_state                  <- function(x, y) {
  
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

Soy.Heartland                   <- filter(Soy, USDAagregion == 9)

Soy.Heartland                   <- Soy.Heartland %>% group_by(.dots = c('Year', 'State')) %>% 
  summarise(Production = sum(SOYBEANS...PRODUCTION..MEASURED.IN.BU, na.rm = TRUE), 
            Planted = sum(Acres_Planted, na.rm = TRUE), 
            Harvested = sum(SOYBEANS...ACRES.HARVESTED, na.rm = TRUE), 
            Half.Planted = mean(Half.Planted.Day, na.rm = TRUE),
            Yield.Rain = mean(NotIrrigated_Yield, na.rm = TRUE), 
            Irrigated.Yield = mean(Yield_Irrigated, na.rm = TRUE), 
            Irrigated.Planted = sum(SOYBEANS..IRRIGATED...ACRES.PLANTED, na.rm = TRUE), 
            Yield = mean(Yield, na.rm = TRUE))

Soy.Heartland                      <- Delta_by_state(Soy.Heartland, 'Yield.Rain')
Soy.Heartland                      <- Delta_by_state(Soy.Heartland, 'Irrigated.Yield')
Soy.Heartland                      <- Delta_by_state(Soy.Heartland, 'Yield')


ggplot(Soy.Heartland, aes(x = Year, y = Irrigated.Planted, colour = State)) + geom_line()
ggplot(Soy.Heartland, aes(x = Year, y = Planted, colour = State)) + geom_line()
ggplot(Soy.Heartland, aes(x = Year, y = Yield, colour = State)) + geom_line()
ggplot(Soy.Heartland, aes(x = Year, y = Half.Planted, colour = State)) + geom_line()

ggplot(Soy.Heartland, aes(x = Irrigated.Planted, y = Yield, colour = State)) + geom_line()

### Interesting - irrigation aint that important  - but when considered on it's own it is sig @ p = 0.0129

fit <- lm(Soy.Total$value[Soy.Total$key == 'Production'] ~ 
            c(NA, Annual.fertiliser.input$total))

