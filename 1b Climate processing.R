
library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(raster)
library(rgdal)
library(zoo)
library(raster)
library(sp)
library(rgdal)
library(stringr)
library(spatialEco)

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Climate')


### Loop through and load

climate_files       <- list.files()
pattern             <- '04_|05_|06_|07_|08_'
climate_files       <- climate_files[c(grep(pattern, climate_files))]
climate_files       <- climate_files[-c(contains('.gz', vars = climate_files))]

for (i in 84:length(climate_files)) {

  setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Climate')
  
  setwd(climate_files[i])

  climate_files2      <- list.files()
  climate_files2      <- climate_files2[-c(contains('.tar', vars = climate_files2))]

  setwd(climate_files2)
  
  base_dir            <- getwd()
  climate_files3      <- list.files()
  climate_files3      <- climate_files3[-c(contains('alaska', vars = climate_files3))]
  
  for (j in 1:length(climate_files3)) {
### read text file
    
    setwd(base_dir)
    
    temp                <- readLines(climate_files3[j])
    temp                <- gsub(' ',',', temp )
    temp                <- gsub(',,', ',', temp)
    temp                <- gsub(',,,', ',', temp)
    temp                <- gsub(',,', ',', temp)
    temp                <- substr(temp, 2, nchar(temp))
  #temp                <- str_split_fixed(temp, pattern = '/////', n = 2)


### split file into data frame

    temp                <- as.data.frame(str_match(temp, "^(.*),(.*),(.*)$"))[, 2:4]
    colnames(temp)      <- c('Lat', 'Long', 'Var')


### change factors into numeric

    unfaccol            <- colwise(function(x) { as.numeric(as.character(x))})
    temp                <- unfaccol(temp)


### make spatial points DF

    temp                <- temp[!is.na(temp$Lat), ]
    temp                <- temp[, c(2, 1, 3)]
    temp_spdf           <- SpatialPointsDataFrame(temp[, 1:2], data = temp, proj4string = CRS("+init=EPSG:4326"))


### get values for target data frame

    temp_spdf                    <- spTransform(temp_spdf, CRS(proj4string(counties)))
    county_climate               <- over(counties, temp_spdf[, 1:3], fn = mean)
    county_climate$countyName    <- counties$NAME
    county_climate$OverallANSI   <- counties$code
    county_climate$agregion      <- map_agregion(county_climate)
    county_climate$State         <- counties$STATEFP

### write out

    folder.name                  <- substr(list.files()[j], 1, 11)

    setwd('E:/Modules/Dissertation/Data/Climate')

    dir.create(file.path(getwd(), folder.name))
    setwd(file.path(getwd(), '/', folder.name)) 

    write.csv(county_climate, paste0(folder.name, '.csv'))
    
    print(i)
    
  }

  
}