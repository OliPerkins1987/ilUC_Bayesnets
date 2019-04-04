

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

setwd('E:/Modules/Dissertation/Data/Processed Dat')


  ### Function to determine delta by timestepf


Delta_by_group                  <- function(x, y) {
  
  t <- list()
  
  for (i in 1:length(levels(factor(x$USDAagregion)))) {
    
    t[[i]] <- data.frame(filter(x, USDAagregion == levels(factor(x$USDAagregion))[i]))
    
  }
  
  s        <- lapply(t, function(x) {x[, y][-1] - x[, y]})
  s        <- lapply(s, function(x) {data.frame(c(0, x[2:length(x)]))})
  
  ### Combine frames
  
  s                          <- rbind.fill(s)
  r                          <- rbind.fill(t)
  r[, ncol(r)+1]             <- s
  colnames(r)[ncol(r)]       <- paste0('Delta_', y)
  
  return(r)
  
}

#############################################################################

### 2) Import and process Soy data

#############################################################################


Soy                             <- read.csv("Processed_Soy.csv", stringsAsFactors = FALSE)
Soy                             <- Soy[, 2:ncol(Soy)]
Soy                             <- fill_na(Soy)
Soy$Yield                       <- Soy$SOYBEANS...PRODUCTION..MEASURED.IN.BU / Soy$Acres_Planted
Soy$Yield_Irrigated             <- Soy$SOYBEANS..IRRIGATED...PRODUCTION..MEASURED.IN.BU / Soy$SOYBEANS..IRRIGATED...ACRES.PLANTED
Soy$NotIrrigated_Planted        <- Soy$Acres_Planted - Soy$SOYBEANS..IRRIGATED...ACRES.PLANTED
Soy$NotIrrigated_Produced       <- Soy$SOYBEANS...PRODUCTION..MEASURED.IN.BU - Soy$SOYBEANS..IRRIGATED...PRODUCTION..MEASURED.IN.BU
Soy$NotIrrigated_Yield          <- Soy$NotIrrigated_Produced / Soy$NotIrrigated_Planted 


Soy.Agregion                    <- Soy %>% group_by(.dots = c('Year', 'USDAagregion')) %>% 
  summarise(Production = sum(SOYBEANS...PRODUCTION..MEASURED.IN.BU), 
            Planted = sum(Acres_Planted), 
            Harvested = sum(SOYBEANS...ACRES.HARVESTED), 
            Half.Planted = mean(Half.Planted.Day),
            Yield.Rain = mean(NotIrrigated_Yield, na.rm = TRUE), 
            Irrigated.Yield = mean(Yield_Irrigated, na.rm = TRUE), 
            Irrigated.Planted = sum(SOYBEANS..IRRIGATED...ACRES.PLANTED))


  ### Make Delta frames

Soy.Agregion <- Delta_by_group(Soy.Agregion, 'Yield.Rain')
Soy.Agregion <- Delta_by_group(Soy.Agregion, 'Irrigated.Yield')


  
  
  
  
