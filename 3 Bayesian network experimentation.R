

library(bnlearn)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)


setwd('E:/Modules/Dissertation/Data/Model Ready')

fill_na              <- colwise(function(x){ifelse(is.na(x), 0, x)})


#####################################################################################

### 1) Read in data

#####################################################################################

commods.flows          <- read.csv('commodityflows_USA.csv')
commods.flows[, 4:17]  <- fill_na(commods.flows[, 4:17])

commods.flows$group    <- ifelse(commods.flows$State %in% c('OHIO', 'IOWA', 
                                                           'ILLINOIS',  
                                                           'INDIANA', 'MISSOURI'), 'Heartland', 
                                 ifelse(commods.flows$State %in% c('MINNESOTA', 'WISCONSIN', 
                                          'MICHIGAN', 'NORTH DAKOTA', 'SOUTH DAKOTA',
                                          'NEBRASKA'), 'Northern', 
                                        
                                        ifelse(commods.flows$State %in% c('MISSISSIPPI', 'ARKANSAS', 'LOUISIANA'), 'Delta', 
                                               
                                               'Prarie')))

commods.flows.grouped  <- commods.flows %>% group_by(.dots = c('group', 'Year')) %>% summarise(Delta_Corn.Planted = 
                                                                            sum(Delta_Corn.Planted, na.rm = TRUE),
                                                                          Delta_RICE...ACRES.PLANTED = sum(Delta_RICE...ACRES.PLANTED, na.rm = TRUE), 
                                                                          Delta_SORGHUM...ACRES.PLANTED = sum(Delta_SORGHUM...ACRES.PLANTED, na.rm = TRUE), 
                                                                          Delta_SUGARBEETS...ACRES.PLANTED = sum(Delta_SUGARBEETS...ACRES.PLANTED, na.rm = TRUE), 
                                                                          Delta_Soy.Planted = sum(Delta_Soy.Planted, na.rm = TRUE), 
                                                                          Delta_WHEAT...ACRES.PLANTED = sum(Delta_WHEAT...ACRES.PLANTED, na.rm = TRUE), 
                                                                          Delta_COTTON...ACRES.PLANTED = sum(Delta_COTTON...ACRES.PLANTED, na.rm = TRUE))

commods.flows.wide     <- list()

for (i in 3:9) {
  
  commods.flows.wide[[i]]                <- spread(commods.flows.grouped[, c(1, 2, i)], key = group, value = colnames(commods.flows.grouped[i]))
  
  colnames(commods.flows.wide[[i]])[2:5] <- paste0(colnames(commods.flows.wide[[i]])[2:5], '_', colnames(commods.flows.grouped[i]))
  
}

commods.flows.wide           <- commods.flows.wide[3:9]
commods.flows.wide           <- bind_cols(commods.flows.wide)
commods.flows.wide[, 2:5]    <- fill_na(commods.flows.wide[, 2:5])
commods.flows.wide           <- commods.flows.wide[, -c(contains('Year', vars = colnames(commods.flows.wide)))]
commods.flows.wide           <- commods.flows.wide[1:27, ]

#####################################################################################

### 2) Screw around with models

#####################################################################################


commods.flows.mod    <- iamb(commods.flows.wide, alpha = 0.5)

predict(commods.flows.mod, node = 'Delta_Northern_Soy.Planted', data = commods.flows.wide)



