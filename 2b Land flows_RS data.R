

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)



### read in data

setwd('E:/Modules/Dissertation/Data/QA/2012 Cropscape')
LC_2012 <- read.csv('USA Landcover 2012.csv', stringsAsFactors = FALSE)

setwd('E:/Modules/Dissertation/Data/QA/2017 Cropscape')
LC_2017 <- read.csv('USALandcover.csv', stringsAsFactors = FALSE)

setwd('E:/Modules/Dissertation/Data/QA')
LC_key  <- read.csv('RS_key.csv')


###################################################################################

### Process

###################################################################################


fill_na             <- colwise(function(x) {ifelse(is.na(x), 0, x)})

LC                  <- LC_2017[1:4845, ]
LC$X2012            <- LC_2012$X2012
LC                  <- fill_na(LC)
LC$Delta            <- LC$X2017 - LC$X2012

LC$X                <- LC_key$ï..Agriculture
colnames(LC)[1]     <- 'Agriculture?'
  

#######################################################################################################

### 1) Analyse data 

#######################################################################################################


LC %>% group_by(`Agriculture?`) %>% summarise(LC_type_2017 = sum(X2017), LC_type_2012 = sum(X2012))

### Trend of different commodities and importance

commods             <- filter(LC, `Agriculture?` == 'Y')
#commods             <- filter(commods, abs(Delta) > 100000)
commods             <- commods %>% group_by(CLASS_NAME) %>% summarise(LC_type_2017 = sum(X2017), LC_type_2012 = sum(X2012))
commods$Delta       <- commods$LC_type_2017 - commods$LC_type_2012
commods$pct_change  <- (commods$Delta / commods$LC_type_2012) * 100

commods$pct_total   <- (commods$LC_type_2017 / sum(commods$LC_type_2017)) * 100
big.commods         <- filter(commods, pct_total > 1)
  