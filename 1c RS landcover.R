

library(raster)
library(stringr)
library(dplyr)
library(foreign)

setwd('E:/Modules/Dissertation/Data/QA/2017 Cropscape')

base.dir  <- getwd()
file.list <- list.files()
Vals      <- data.frame()

#######################################################################################

### processing pipeline

#######################################################################################

for (i in 11:length(file.list)) {

  setwd(base.dir)
  
  state.list   <- list.files()
  state.list   <- substr(state.list, 6, 7)
  
  setwd(list.files()[i])

  r            <- raster(list.files()[1])
  meta         <- read.dbf(list.files()[2])
  vals         <- r[,]
  
  vals         <- vals[vals != 0 & vals != 63 & vals != 81 &vals != 83 &vals != 195 &vals != 190 & vals != 152]
  vals_counts  <- table(vals)

  meta$`2017`  <- as.numeric(sapply(meta$VALUE, function(x) {as.vector(vals_counts)[which(names(vals_counts) == x)]}))
  meta$State   <- state.list[i]
  
  Vals         <- rbind(Vals, meta)
  print(i)
  remove(r)
  remove(meta)
  gc()
  
}
