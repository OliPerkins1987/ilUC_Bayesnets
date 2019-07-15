
###################################################################################
#
#   Script imports and cleans data for further analysis and modelling
#
###################################################################################

library(imputeTS)
library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(raster)
library(rgdal)
library(zoo)
library(DMwR)



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

County_FIPS              <- read.csv('County to FIPS.csv')


  ### function to map counties to ag regions


assign_other_county      <- function(x) {
  
  for (i in 1:nrow(x)){
    
    if(x$County[i] == 'OTHER (COMBINED) COUNTIES')
      
      x$County.ANSI[i] <- x$County.ANSI[i-1]
    
  }
  
  return(x$County.ANSI)
  
}


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
Soy_planted$County.ANSI  <- assign_other_county(Soy_planted)
Soy_planted$OverallANSI  <- paste0(Soy_planted$State.ANSI, Soy_planted$County.ANSI)
Soy_planted$USDAagregion <- map_agregion(Soy_planted)
  
  #apply(Soy_planted$OverallANSI, 2, function(x) {counties_key_list[which(county_to_ag_region$ï..Fips == x)]})

Soy_planted_national     <- Soy_planted %>% group_by(Year) %>% summarise(harvested = sum(Value))
Soy_planted_district     <- Soy_planted %>% group_by(.dots = c('Ag.District', 'Year')) %>% summarise(harvested = sum(Value))
Soy_planted_agregion    <- Soy_planted %>% group_by(.dots = c('Year', 'USDAagregion')) %>% summarise(planted = sum(Value))

#ggplot(Soy_planted_agregion, aes(x = Year, y = planted, colour = factor(USDAagregion))) +geom_line()


  ### b) production

Soy_production          <- read.csv('Soy production_1990_2017.csv')
Soy_production$Value[Soy_production$Value == ' (D)'] <- 0

Soy_production$Value    <- as.numeric(gsub(',', '', as.character(Soy_production$Value)))
#Soy_production$Value    <- Soy_production$Value / 36.74371 ### convert to metric tonnes
Soy_production          <- Soy_production[Soy_production$Program == 'SURVEY', ]

Soy_production$County.ANSI  <- formatC(Soy_production$County.ANSI, width = 3, format = "d", flag = "0")
Soy_production$County.ANSI  <- assign_other_county(Soy_production)
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

Soy_Irrigated_Planted              <- read.csv('Soybean Irrigated Planted Area.csv')
Soy_Irrigated_Planted$Value[Soy_Irrigated_Planted$Value ==' (D)'] <- 0

Soy_Irrigated_Planted$Value        <- as.numeric(gsub(',', '', as.character(Soy_Irrigated_Planted$Value)))
Soy_Irrigated_Planted$Value        <- as.numeric(as.character(Soy_Irrigated_Planted$Value))
Soy_Irrigated_Planted$Value[is.na(Soy_Irrigated_Planted$Value ==' (D)')] <- 0

Soy_Irrigated_Planted$County.ANSI  <- formatC(Soy_Irrigated_Planted$County.ANSI, width = 3, format = "d", flag = "0")
Soy_Irrigated_Planted$County.ANSI  <- assign_other_county(Soy_Irrigated_Planted)
Soy_Irrigated_Planted$OverallANSI  <- paste0(Soy_Irrigated_Planted$State.ANSI, Soy_Irrigated_Planted$County.ANSI)
Soy_Irrigated_Planted$USDAagregion <- map_agregion(Soy_Irrigated_Planted)

Soy_Irrigated_Planted_State        <- Soy_Irrigated_Planted %>% group_by(Year, State) %>% summarise(planted = sum(Value, na.rm = TRUE))
##Soy_Irrigated_Area %>% group_by(Year) %>% summarise(total = sum(Value)) significant increase over time

ggplot(Soy_Irrigated_Planted_State, aes(x = Year, y = planted, colour = State)) + geom_line()


  ### f) Biodiesel price vs diesel

Biodiesel_economics            <- read.csv('Biodiesel Economics.csv', skip = 3)
Biodiesel_economics            <- Biodiesel_economics[1:146, 1:3]
Biodiesel_economics$Month.yr   <- gsub('-', ' ', Biodiesel_economics$Month.yr)
Biodiesel_economics$Month.yr   <- gsub(' ', ' 20', Biodiesel_economics$Month.yr)
Biodiesel_economics$Month.yr   <- as.yearmon(Biodiesel_economics$Month.yr[1], format = '%b %Y')

  ### g) Harvested area

Soy_Harvested               <- read.csv('Soy_Harvested_Total.csv')
Soy_Harvested$Value[Soy_Harvested$Value ==' (D)'] <- 0

Soy_Harvested$Value         <- as.numeric(gsub(',', '', as.character(Soy_Harvested$Value)))
Soy_Harvested$Value         <- as.numeric(as.character(Soy_Harvested$Value))
#Soy_Harvested$Value[is.na(Soy_Harvested$Value ==' (D)')] <- 0

Soy_Harvested$County.ANSI   <- formatC(Soy_Harvested$County.ANSI, width = 3, format = "d", flag = "0")
Soy_Harvested$County.ANSI   <- assign_other_county(Soy_Harvested)
Soy_Harvested$OverallANSI   <- paste0(Soy_Harvested$State.ANSI, Soy_Harvested$County.ANSI)
Soy_Harvested$USDAagregion  <- map_agregion(Soy_Harvested)

Soy_Harvested_State         <- Soy_Harvested %>% group_by(Year, State) %>% summarise(Harvested = sum(Value))


  ### h) Harvested area Irrigated

Soy_Irrigated_Harvested              <- read.csv('Soy_Irrigated_Harvested.csv')
Soy_Irrigated_Harvested$Value[Soy_Irrigated_Harvested$Value ==' (D)'] <- 0

Soy_Irrigated_Harvested$Value        <- as.numeric(gsub(',', '', as.character(Soy_Irrigated_Harvested$Value)))
Soy_Irrigated_Harvested$Value        <- as.numeric(as.character(Soy_Irrigated_Harvested$Value))
Soy_Irrigated_Harvested$Value[is.na(Soy_Irrigated_Harvested$Value ==' (D)')] <- 0

Soy_Irrigated_Harvested$County.ANSI  <- formatC(Soy_Irrigated_Harvested$County.ANSI, width = 3, format = "d", flag = "0")
Soy_Irrigated_Harvested$County.ANSI  <- assign_other_county(Soy_Irrigated_Harvested)
Soy_Irrigated_Harvested$OverallANSI  <- paste0(Soy_Irrigated_Harvested$State.ANSI, Soy_Irrigated_Harvested$County.ANSI)
Soy_Irrigated_Harvested$USDAagregion <- map_agregion(Soy_Irrigated_Harvested)

Soy_Irrigated_Harvested_agregion     <- Soy_Irrigated_Harvested %>% group_by(Year, USDAagregion) %>% summarise(Harvested = sum(Value))

ggplot(Soy_Irrigated_Harvested_agregion, aes(x = as.numeric(Year), y = Harvested, colour = factor(USDAagregion))) + geom_line()


  ### i) Double crop

Soy_Double_crop                      <- read.csv('Soy Double Crop.csv')
Soy_Double_crop $Value[Soy_Double_crop $Value ==' (D)'] <- 0

Soy_Double_crop$Value                <- as.numeric(as.character(Soy_Double_crop $Value))
Soy_Double_crop$Value[is.na(Soy_Double_crop $Value ==' (D)')] <- 0

Soy_Double_crop$County.ANSI          <- formatC(Soy_Double_crop$County.ANSI, width = 3, format = "d", flag = "0")


  ### This needs fixing - the double cropping is done by state ag region not county

#Soy_Double_crop$County.ANSI  <- assign_other_county(Soy_Double_crop)
#Soy_Double_crop $OverallANSI  <- paste0(Soy_Double_crop $State.ANSI, Soy_Double_crop$County.ANSI)
#Soy_Double_crop $USDAagregion <- map_agregion(Soy_Double_crop )


  ### j) Planting date

Soy_planting_date             <- read.csv('Soy_planted date_state_1990_2017.csv')
Soy_planting_date$Week.Ending <- as.Date(Soy_planting_date$Week.Ending, format = '%d/%m/%Y')


### date at which 50% was planted

planted.date.list <- list()
Soy_half_planted  <- list()

for (i in 1:length(levels(Soy_planting_date$State))) {
  
  for (j in 1:length(unique(Soy_planting_date$Year))) {
    
    planted.date.list[[j]] <- Soy_planting_date$Week.Ending[Soy_planting_date$State == levels(Soy_planting_date$State)[i] & Soy_planting_date$Year == unique(Soy_planting_date$Year)[j] & Soy_planting_date$Value > 50]
    
  }
  
  Soy_half_planted[[i]]    <- planted.date.list
  
}

names(Soy_half_planted)          <- levels(Soy_planting_date$State)
Soy_half_planted_test            <- lapply(Soy_half_planted, function (x) {lapply(x, min)})
planted_results                  <- data.frame()

Soy_half_planted_test            <- lapply(Soy_half_planted_test, function(x) {as.Date(unlist(x))})
Soy_half_planted_test            <- data.frame(Soy_half_planted_test)
rownames(Soy_half_planted_test)  <- c(2018:1990)


Soy_half_planted                 <- gather(Soy_half_planted_test)
Soy_half_planted$Year            <- c(2018:1990)
Soy_half_planted$value           <- format(Soy_half_planted$value, "%j")
colnames(Soy_half_planted)[1:2]  <- c('State', 'Half Planted Day')

planted.dates.Soy <- Soy_half_planted %>% group_by(Year) %>% summarise(planted = mean(as.numeric(value), na.rm = TRUE))


  ### k) Irrigated Production

Soy_Irrigated_Production              <- read.csv('Soybeans Irrigated Production.csv')
Soy_Irrigated_Production$Value        <- as.numeric(gsub(',', '', as.character(Soy_Irrigated_Production$Value)))
Soy_Irrigated_Production$Value        <- as.numeric(as.character(Soy_Irrigated_Production$Value))
Soy_Irrigated_Production$Value[is.na(Soy_Irrigated_Production$Value ==' (D)')] <- 0

Soy_Irrigated_Production$County.ANSI  <- formatC(Soy_Irrigated_Production$County.ANSI, width = 3, format = "d", flag = "0")
Soy_Irrigated_Production$County.ANSI  <- assign_other_county(Soy_Irrigated_Production)
Soy_Irrigated_Production$OverallANSI  <- paste0(Soy_Irrigated_Production$State.ANSI, Soy_Irrigated_Production$County.ANSI)
Soy_Irrigated_Production$USDAagregion <- map_agregion(Soy_Irrigated_Production)

Soy_Irrigated_Production_agregion     <- Soy_Irrigated_Production %>% group_by(Year, USDAagregion) %>% summarise(Produced = sum(Value))




  ### z) Merge data


Category_merge                   <- function(x, y) {
  
  z <- vector()
  
  for (i in 1:5) {
    
    for (j in 1:7147) {
      
      if (x$State[i] == y$State[j] & x$Year[i] == y$Year[j]) {
      
        z[i] <- y$Value[j]
      
      }
        
    }
    
  }
  
  return(z)
  
}


Soy.frame                        <- Soy_planted[, c('Year', 'Geo.Level', 'State', 'State.ANSI', 'Ag.District', 'Ag.District.Code', 'County', 'County.ANSI', 'OverallANSI')]
Soy.frame$Acres_Planted          <- Soy_planted$Value

Soy.frame                        <- data.frame(filter(Soy.frame, County != 'OTHER (COMBINED) COUNTIES'))

Soy.frame                        <- merge(Soy.frame, Soy_Harvested, by = c('Year','State', 'County'), all.x = TRUE)
colnames(Soy.frame)[22]          <- as.character(Soy_Harvested$Data.Item[1])
Soy.frame                        <- data.frame(filter(Soy.frame, County != 'OTHER (COMBINED) COUNTIES'))

Soy.frame                        <- Soy.frame[, c('Year', 'Geo.Level.x', 'State', 'State.ANSI.x',
                                                  'Ag.District', 'Ag.District.Code', 'County', 'County.ANSI',
                                                  'OverallANSI', 'USDAagregion', 'Acres_Planted', colnames(Soy.frame)[22])]

Soy.frame                        <- merge(Soy.frame, Soy_production[, c('Year', 'OverallANSI', 'Value', 'County')], by = c('Year', 'OverallANSI', 'County'), all.x = TRUE)
colnames(Soy.frame)[13]          <- as.character(Soy_production$Data.Item[1])

Soy.frame                        <- merge(Soy.frame, Soy_price[Soy_price$Period == 'MARKETING YEAR', c('Year', 'State', 'Value')], by = c('Year', 'State'))  
colnames(Soy.frame)[14]          <- as.character(Soy_price$Data.Item[1])

Soy.frame                        <- merge(Soy.frame, Soy_half_planted, by = c('Year', 'State'), all.x = TRUE)

Soy.frame                        <- merge(Soy.frame, Soy_Irrigated_Planted[, c('Year', 'OverallANSI', 'Value', 'County')], by = c('Year', 'OverallANSI', 'County'), all.x = TRUE)
colnames(Soy.frame)[16]          <- as.character(Soy_Irrigated_Planted$Data.Item[1])

Soy.frame                        <- merge(Soy.frame, Soy_Irrigated_Harvested[, c('Year', 'OverallANSI', 'Value', 'County')], by = c('Year', 'OverallANSI', 'County'), all.x = TRUE)
colnames(Soy.frame)[17]          <- as.character(Soy_Irrigated_Harvested$Data.Item[1])

Soy.frame                        <- merge(Soy.frame, Soy_Irrigated_Production[, c('Year', 'OverallANSI', 'Value', 'County')], by = c('Year', 'OverallANSI', 'County'), all.x = TRUE)
colnames(Soy.frame)[18]          <- as.character(Soy_Irrigated_Production$Data.Item[1])

dupes                            <- duplicated(Soy.frame[, c("Year", "State", "County", "Ag.District")])
dupes                            <- Soy.frame[dupes == TRUE & Soy.frame$County != 'OTHER (COMBINED) COUNTIES', ]

Soy.counties                     <- Soy.frame



  ### 1za) Delta (annual change by state)

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
Corn_planted$County.ANSI  <- assign_other_county(Corn_planted)
Corn_planted$OverallANSI  <- paste0(Corn_planted$State.ANSI, Corn_planted$County.ANSI)
Corn_planted$USDAagregion <- map_agregion(Corn_planted)

Corn_planted_national <- Corn_planted %>% group_by(Year) %>% summarise(planted = sum(Value))
Corn_planted_state    <- Corn_planted %>% group_by(State) %>% summarise(planted = sum(Value))

Corn_planted_agregion   <- Corn_planted %>% group_by(.dots = c('Year', 'USDAagregion')) %>% summarise(planted = sum(Value))

ggplot(Corn_planted_agregion, aes(x = Year, y = planted, colour = factor(USDAagregion))) + geom_line()

  ### b) harvested area

Corn1_harvested       <- read.csv('Corn Harvested Area.csv')
Corn2_harvested       <- read.csv('Corn Harvested Area_2ndpart.csv')
Corn_harvested        <- rbind(Corn1_harvested ,Corn2_harvested)

remove(Corn1_harvested, Corn2_harvested)

Corn_harvested        <- Corn_harvested[Corn_harvested$Program != 'CENSUS', ]
Corn_harvested        <- Corn_harvested[Corn_harvested$Period == 'YEAR', ]
Corn_harvested$Value  <- as.numeric(gsub(',', '', Corn_harvested$Value))

Corn_harvested$County.ANSI   <- formatC(Corn_harvested$County.ANSI, width = 3, format = "d", flag = "0")
Corn_harvested$County.ANSI   <- assign_other_county(Corn_harvested)
Corn_harvested $OverallANSI  <- paste0(Corn_harvested$State.ANSI, Corn_harvested$County.ANSI)
Corn_harvested $USDAagregion <- map_agregion(Corn_harvested)

Corn_harvested_national <- Corn_harvested %>% group_by(Year) %>% summarise(harvested = sum(Value))
Corn_harvested_state    <- Corn_harvested %>% group_by(State) %>% summarise(harvested = sum(Value))


  ### c) production

Corn1_produced        <- read.csv('Corn_produced1.csv')
Corn2_produced        <- read.csv('Corn_produced2.csv')
Corn_produced         <- rbind(Corn1_produced ,Corn2_produced)

remove(Corn1_produced , Corn2_produced)

Corn_produced         <- Corn_produced[Corn_produced$Program != 'CENSUS', ]
Corn_produced         <- Corn_produced[Corn_produced$Period == 'YEAR', ]
Corn_produced$Value   <- as.numeric(gsub(',', '', as.character(Corn_produced$Value)))
Corn_produced$Value   <- Corn_produced$Value / 39.368

Corn_produced$County.ANSI   <- formatC(Corn_produced$County.ANSI, width = 3, format = "d", flag = "0")
Corn_produced$County.ANSI   <- assign_other_county(Corn_produced)
Corn_produced$OverallANSI   <- paste0(Corn_produced$State.ANSI, Corn_produced$County.ANSI)
Corn_produced$USDAagregion  <- map_agregion(Corn_produced)

Corn_produced_national <- Corn_produced %>% group_by(Year) %>% summarise(produced = sum(Value))
Corn_produced_state    <- Corn_produced %>% group_by(State) %>% summarise(produced = sum(Value))
Corn_produced_agregion <- Corn_produced %>% group_by(USDAagregion) %>% summarise(produced = sum(Value))


  ### d) price

Corn_price              <-  read.csv('Corn_price.csv')
Corn_price$Value        <-  as.numeric(as.character(Corn_price$Value))

Corn_price$County.ANSI  <- formatC(Corn_price$County.ANSI, width = 3, format = "d", flag = "0")
Corn_price$County.ANSI  <- assign_other_county(Corn_price)
Corn_price$OverallANSI  <- paste0(Corn_price$State.ANSI, Corn_produced$County.ANSI)
Corn_price$USDAagregion <- map_agregion(Corn_price)

Corn_price              <- Corn_price[Corn_price$Period == 'MARKETING YEAR', ]

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

Corn_Irrigated_Planted              <- read.csv('Corn Irrigated Area planted.csv')
Corn_Irrigated_Planted$Value[Corn_Irrigated_Planted$Value ==' (D)'] <- 0
Corn_Irrigated_Planted$Value        <- as.numeric(as.character(gsub(',', '', Corn_Irrigated_Planted$Value)))

Corn_Irrigated_Planted$County.ANSI  <- formatC(Corn_Irrigated_Planted$County.ANSI, width = 3, format = "d", flag = "0")
Corn_Irrigated_Planted$County.ANSI  <- assign_other_county(Corn_Irrigated_Planted)
Corn_Irrigated_Planted$OverallANSI  <- paste0(Corn_Irrigated_Planted$State.ANSI, Corn_Irrigated_Planted$County.ANSI)
Corn_Irrigated_Planted$USDAagregion <- map_agregion(Corn_Irrigated_Planted)

Corn_Irrigated_Planted$Value        <- as.numeric(as.character(Corn_Irrigated_Planted$Value))
Corn_Irrigated_Planted$Value[is.na(Corn_Irrigated_Planted$Value ==' (D)')] <- 0
Corn_Irrigated_Planted_State        <- Corn_Irrigated_Planted %>% group_by(Year, State) %>% summarise(total = sum(Value))
Corn_Irrigated_Planted %>% group_by(Year) %>% summarise(total = sum(Value)) ###significant increase over time


  ### h) Irrigated Harvested Area

Corn_Irrigated_Harvested              <- read.csv('Corn_Irrigated_Harvested.csv')
Corn_Irrigated_Harvested$Value[Corn_Irrigated_Harvested$Value ==' (D)'] <- 0
Corn_Irrigated_Harvested$Value        <- as.numeric(as.character(gsub(',', '', Corn_Irrigated_Harvested$Value)))

Corn_Irrigated_Harvested$County.ANSI  <- formatC(Corn_Irrigated_Harvested$County.ANSI, width = 3, format = "d", flag = "0")
Corn_Irrigated_Harvested$County.ANSI  <- assign_other_county(Corn_Irrigated_Harvested)
Corn_Irrigated_Harvested$OverallANSI  <- paste0(Corn_Irrigated_Harvested$State.ANSI, Corn_Irrigated_Harvested$County.ANSI)
Corn_Irrigated_Harvested$USDAagregion <- map_agregion(Corn_Irrigated_Harvested)

Corn_Irrigated_Harvested$Value        <- as.numeric(as.character(Corn_Irrigated_Harvested$Value))
Corn_Irrigated_Harvested$Value[is.na(Corn_Irrigated_Harvested$Value ==' (D)')] <- 0
Corn_Irrigated_Harvested_State        <- Corn_Irrigated_Harvested %>% group_by(Year, State) %>% summarise(total = sum(Value))
Corn_Irrigated_Harvested %>% group_by(Year) %>% summarise(total = sum(Value)) ###significant increase over time


  ### i) Planting Date

Corn_planting_date             <- read.csv('Corn planting date.csv')
Corn_planting_date$Week.Ending <- as.Date(Corn_planting_date$Week.Ending, format = '%d/%m/%Y')


### date at which 50% was planted

planted.date.list <- list()
Corn_planting_date2  <- list()

for (i in 1:length(levels(Corn_planting_date$State))) {
  
  for (j in 1:length(unique(Corn_planting_date$Year))) {
    
    planted.date.list[[j]] <- Corn_planting_date$Week.Ending[Corn_planting_date$State == levels(Corn_planting_date$State)[i] & Corn_planting_date$Year == unique(Corn_planting_date$Year)[j] & Corn_planting_date$Value > 50]
    
  }
  
  Corn_planting_date2[[i]]    <- planted.date.list
  
}

names(Corn_planting_date2)       <- levels(Corn_planting_date$State)
Corn_planting_date2              <- lapply(Corn_planting_date2, function (x) {lapply(x, min)})

Corn_planting_date2            <- lapply(Corn_planting_date2, function(x) {as.Date(unlist(x))})
Corn_planting_date2            <- data.frame(Corn_planting_date2)
rownames(Corn_planting_date2)  <- c(2018:1990)

Corn_half_planted                 <- gather(Corn_planting_date2)
Corn_half_planted$Year            <- c(2018:1990)
Corn_half_planted$value           <- format(Corn_half_planted$value, "%j")
colnames(Corn_half_planted)[1]    <- 'State'

planted.dates.Corn <- Corn_half_planted %>% group_by(Year) %>% summarise(planted = mean(as.numeric(value), na.rm = TRUE))


  ### j) Irrigated Production


Corn_Irrigated_Produced              <- read.csv('Corn Irrigated Produced.csv')
Corn_Irrigated_Produced$Value[Corn_Irrigated_Produced$Value ==' (D)'] <- 0
Corn_Irrigated_Produced$Value        <- as.numeric(as.character(gsub(',', '', Corn_Irrigated_Produced$Value)))

Corn_Irrigated_Produced$County.ANSI  <- formatC(Corn_Irrigated_Produced$County.ANSI, width = 3, format = "d", flag = "0")
Corn_Irrigated_Produced$County.ANSI  <- assign_other_county(Corn_Irrigated_Produced)
Corn_Irrigated_Produced$OverallANSI  <- paste0(Corn_Irrigated_Produced$State.ANSI, Corn_Irrigated_Produced$County.ANSI)
Corn_Irrigated_Produced$USDAagregion <- map_agregion(Corn_Irrigated_Produced)

Corn_Irrigated_Produced$Value        <- as.numeric(as.character(Corn_Irrigated_Produced$Value))
Corn_Irrigated_Produced$Value[is.na(Corn_Irrigated_Produced$Value ==' (D)')] <- 0
#Corn_Irrigated_Produced        <- Corn_Irrigated_Produced %>% group_by(Year, State) %>% summarise(total = sum(Value))
#Corn_Irrigated_Produced %>% group_by(Year) %>% summarise(total = sum(Value)) ###significant increase over time


  ### k) Irrigated Yield

Corn_Irrigated_Yield             <- read.csv('Corn_Yield_Irrigated.csv')
Corn_Irrigated_Yield$Value[Corn_Irrigated_Yield$Value ==' (D)'] <- 0
Corn_Irrigated_Yield$Value        <- as.numeric(as.character(gsub(',', '', Corn_Irrigated_Yield$Value)))

Corn_Irrigated_Yield$County.ANSI  <- formatC(Corn_Irrigated_Yield$County.ANSI, width = 3, format = "d", flag = "0")
Corn_Irrigated_Yield$County.ANSI  <- assign_other_county(Corn_Irrigated_Yield)
Corn_Irrigated_Yield$OverallANSI  <- paste0(Corn_Irrigated_Yield$State.ANSI, Corn_Irrigated_Yield$County.ANSI)
Corn_Irrigated_Yield$USDAagregion <- map_agregion(Corn_Irrigated_Yield)

Corn_Irrigated_Yield$Value        <- as.numeric(as.character(Corn_Irrigated_Yield$Value))
Corn_Irrigated_Yield$Value[is.na(Corn_Irrigated_Yield$Value ==' (D)')] <- 0            


  ### l) Overall Yield

Corn_Yield             <- read.csv('Corn Yield.csv')
Corn_Yield$Value[Corn_Yield$Value ==' (D)'] <- 0
Corn_Yield$Value        <- as.numeric(as.character(gsub(',', '', Corn_Yield$Value)))

Corn_Yield$County.ANSI  <- formatC(Corn_Yield$County.ANSI, width = 3, format = "d", flag = "0")
Corn_Yield$County.ANSI  <- assign_other_county(Corn_Yield)
Corn_Yield$OverallANSI  <- paste0(Corn_Yield$State.ANSI, Corn_Yield$County.ANSI)
Corn_Yield$USDAagregion <- map_agregion(Corn_Yield)

Corn_Yield$Value        <- as.numeric(as.character(Corn_Yield$Value))
Corn_Yield$Value[is.na(Corn_Yield$Value ==' (D)')] <- 0            



### z) Merge data

Corn.frame                       <- Corn_planted[, c('Year', 'Geo.Level', 'State', 'State.ANSI', 'Ag.District', 'Ag.District.Code', 'County', 'County.ANSI', 'OverallANSI')]
Corn.frame$Acres_Planted         <- Corn_planted$Value

Corn.frame                       <- data.frame(filter(Corn.frame, County != 'OTHER (COMBINED) COUNTIES'))

Corn.frame                       <- merge(Corn.frame, Corn_harvested, all.x = TRUE)
colnames(Corn.frame)[22]         <- as.character(Corn_harvested$Data.Item[1])
Corn.frame                        <- Corn.frame[, c('Year', 'Geo.Level', 'State', 'State.ANSI',
                                                  'Ag.District', 'Ag.District.Code', 'County', 'County.ANSI',
                                                  'OverallANSI', 'USDAagregion', 'Acres_Planted', colnames(Corn.frame)[22])]

Corn.frame                        <- merge(Corn.frame, Corn_produced[, c('Year', 'OverallANSI', 'County', 'Value')], by = c('Year', 'County', 'OverallANSI'), all.x = TRUE)
colnames(Corn.frame)[13]          <- as.character(Corn_produced$Data.Item[1])

Corn.frame                        <- merge(Corn.frame, Corn_price[, c('Year', 'State', 'Value')], by = c('Year', 'State'), all.y = TRUE)  
colnames(Corn.frame)[14]          <- as.character(Corn_price$Data.Item[1])

Corn.frame                        <- merge(Corn.frame, Corn_half_planted, by = c('Year', 'State'), all.x = TRUE)

Corn.frame                        <- merge(Corn.frame, Corn_Irrigated_Planted[, c('Year', 'OverallANSI', 'County', 'Value')], by = c('Year', 'County', 'OverallANSI'), all.x = TRUE)
colnames(Corn.frame)[16]          <- as.character(Corn_Irrigated_Planted$Data.Item[1])

Corn.frame                        <- merge(Corn.frame, Corn_Irrigated_Harvested[, c('Year', 'OverallANSI', 'County', 'Value')], by = c('Year', 'County', 'OverallANSI'), all.x = TRUE)
colnames(Corn.frame)[17]          <- as.character(Corn_Irrigated_Harvested$Data.Item[1])

Corn.frame                        <- merge(Corn.frame, Corn_Irrigated_Produced[, c('Year', 'OverallANSI', 'County', 'Value')], by = c('Year', 'County', 'OverallANSI'), all.x = TRUE)
colnames(Corn.frame)[18]          <- as.character(Corn_Irrigated_Produced$Data.Item[1])

Corn.frame                        <- merge(Corn.frame, Corn_Irrigated_Yield[, c('Year', 'OverallANSI', 'County', 'Value')], by = c('Year', 'County', 'OverallANSI'), all.x = TRUE)
colnames(Corn.frame)[19]          <- as.character(Corn_Irrigated_Yield$Data.Item[1])

Corn.frame                        <- merge(Corn.frame, Corn_Yield[, c('Year', 'OverallANSI', 'County', 'Value')], by = c('Year', 'County', 'OverallANSI'), all.x = TRUE)
colnames(Corn.frame)[20]          <- as.character(Corn_Yield$Data.Item[1])


dupes                            <- duplicated(Corn.frame[, c("Year", "State", "County", "Ag.District")])
dupes                            <- Corn.frame[dupes == TRUE,  ]

dupes2 <- duplicated(dupes[, c('Year', 'County', 'Ag.District', 'State')])
dupes2 <- dupes[dupes2 == TRUE,]

Corn.annual <- Corn.frame %>% group_by(Year) %>% summarise(produced = sum(`CORN, GRAIN - PRODUCTION, MEASURED IN BU`, na.rm = TRUE), planted = sum(Acres_Planted,na.rm = TRUE))
ggplot(Corn.annual, aes(x = Year, y = planted)) + geom_line()

#& Corn.frame$County != 'OTHER (COMBINED) COUNTIES',


##################################################################################

### 3) Climate

##################################################################################


  ### Prec & Temp - preprocessed elsewhere



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
colnames(CRP)[3]        <- 'OverallANSI'
CRP$USDAagregion        <- map_agregion(CRP)

CRP.long                <- gather(CRP[, 4:35])
CRP.long$OverallANSI    <- CRP$OverallANSI
colnames(CRP.long)      <- c('Year', 'CRP_Acreage', 'OverallANSI')

CRP_payment             <- read.csv('CRP payment amount.csv', skip = 3)
colnames(CRP_payment)   <- c('State', 'County', 'ID', c(1986:2017))
CRP_payment[,4:35]      <- apply(CRP_payment[, 4:35], 2, function(x){as.numeric(gsub(',', '', x))})

CRP.p.long              <- gather(CRP_payment[, 4:35])
CRP.p.long$OverallANSI  <- CRP$OverallANSI
colnames(CRP.p.long)     <- c('Year', 'CRP_Acreage', 'OverallANSI')

CRP.long$Payment        <- CRP.p.long$CRP_Acreage

#CRP_payment_long       <- gather(CRP_payment[4:35])
#CRP_payment_long       <- aggregate(value ~ key, data = CRP_payment_long, FUN = sum)
#CRP_payment_long$key   <- as.numeric(as.character(CRP_payment_long$key))
#ggplot(CRP_payment_long, aes(x = key, y = value)) + geom_line() #+ scale_y_continuous(limits = c(0, 40000000))

#CRP_long       <- gather(CRP[4:35])
#CRP_long       <- aggregate(value ~ key, data = CRP_long, FUN = sum)
#CRP_long$key   <- as.numeric(as.character(CRP_long$key))
#ggplot(CRP_long, aes(x = key, y = value)) + geom_line() + scale_y_continuous(limits = c(0, 40000000))

### was the amount paid in CRP reduced?


  ### b) Intensification


Inflation.correct                     <- data.frame(inflation = c(1.92, 1.84, 1.79, 1.74,1.69, 1.65, 1.60, 1.56,
                                                                  1.54, 1.51, 1.46, 1.42, 1.40, 1.36, 1.33, 1.29, 
                                                                  1.25, 1.21, 1.17, 1.17, 1.15, 1.12,1.09, 1.08, 1.06, 
                                                                  1.06,1.05, 1.02, 1), Year = c(1990:2018))

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Intensification')



  ### b1) Fertiliser inputs by agricultural system (CENSUS)

Fertiliser_byIrrigation_Census        <- read.csv('Fertiliser_byIrrigation_$_Census.csv')
Fertiliser_byIrrigation_Census$Value  <- as.numeric(gsub(',', '', Fertiliser_byIrrigation_Census$Value))

Fertiliser_byIrrigation_Census_Totals <- filter(Fertiliser_byIrrigation_Census, State == 'US TOTAL')
Fertiliser_byIrrigation_Census_States <- filter(Fertiliser_byIrrigation_Census, State != 'US TOTAL')



### correct for inflation

Fertiliser_byIrrigation_Census_Totals$Value[Fertiliser_byIrrigation_Census_Totals$Year == 1997] <-  Fertiliser_byIrrigation_Census_Totals$Value[Fertiliser_byIrrigation_Census_Totals$Year == 1997] * 1.43
Fertiliser_byIrrigation_Census_Totals$Value[Fertiliser_byIrrigation_Census_Totals$Year == 2002] <-  Fertiliser_byIrrigation_Census_Totals$Value[Fertiliser_byIrrigation_Census_Totals$Year == 2002] * 1.28
Fertiliser_byIrrigation_Census_Totals$Value[Fertiliser_byIrrigation_Census_Totals$Year == 2007] <-  Fertiliser_byIrrigation_Census_Totals$Value[Fertiliser_byIrrigation_Census_Totals$Year == 2007] * 1.11


Fertiliser_byIrrigation_Census_Totals %>% group_by(Year) %>% summarise(total = sum(Value))

Fertiliser_byIrrigation_Census_Totals$AnnualTotal <- c(rep(33022071000,
                                                           times = 3), rep(23574996960, times = 3),
                                                       rep(14974112000, times = 3), rep(17106955580,  times = 3))

Fertiliser_byIrrigation_Census_Totals$Proportion  <- Fertiliser_byIrrigation_Census_Totals$Value / Fertiliser_byIrrigation_Census_Totals$AnnualTotal

ggplot(Fertiliser_byIrrigation_Census_Totals, aes(x = Year, y = Value, colour = Domain.Category)) + geom_line()

anova(lm(Fertiliser_byIrrigation_Census_Totals$Value ~ Fertiliser_byIrrigation_Census_Totals$Domain.Category))



############ *** clear increase in fertiliser inputs by year *** ?? BUT IS THIS JUST TO DO WITH 2007?

### Also expense on fertiliser seems to be more 



  ### b2) Fertiliser inputs by region

Fertiliser_Expense_State        <- read.csv('Fertiliser_Expense_Region.csv')
Fertiliser_Expense_State$Value  <- as.numeric(gsub(',', '', Fertiliser_Expense_State$Value))

Fertiliser_Expense_State$Value  <- apply(Fertiliser_Expense_State, 1, function(x) {as.numeric(x[20]) * Inflation.correct$inflation[which(Inflation.correct$Year == x[2])]})
teg <- Fertiliser_Expense_State %>% group_by(Year) %>% summarise(total = sum(Value))



  ### c) Land value

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Agricultural context')

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


  ### e) Other crops 

### Planted

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Additional Commodities')

Cotton_planted <- read.csv('Cotton_Planted.csv')
Rice           <- read.csv('Rice.csv')
Sorghum        <- read.csv('Sorghum.csv')
Sugar_Beet     <- read.csv('Sugar Beet planted.csv')
Wheat          <- read.csv('Wheat planted.csv')
Barley         <- read.csv('Barley.csv')

extra_commods              <- list(Cotton_planted, Rice, Sorghum, Sugar_Beet, Wheat, Barley)
extra_commods              <- rbind.fill(extra_commods)

extra_commods$Value        <- as.numeric(gsub(',', '', extra_commods$Value))

extra_commods$County.ANSI  <- formatC(extra_commods$County.ANSI, width = 3, format = "d", flag = "0")
extra_commods$County.ANSI  <- assign_other_county(extra_commods)
extra_commods$OverallANSI  <- paste0(extra_commods$State.ANSI, extra_commods$County.ANSI)
extra_commods$USDAagregion <- map_agregion(extra_commods)

extra_commods              <- extra_commods[, -c(4, 12:15, 18:19, 21)]

extra_commods              <- data.frame(extra_commods %>% filter(Period == 'YEAR' & Geo.Level != 'NATIONAL'))

extra_commods.state        <- extra_commods %>% dplyr::group_by(.dots = c('Year', 'State', 'Commodity')) %>%
                                  dplyr::summarise(Acres.Planted = sum(Value, na.rm = TRUE))

setwd('E:/Modules/Dissertation/Data/Processed Dat')

#write.csv(extra_commods.state, 'extra_commods_barley.csv')

##################################################################### 

##add pastured cropland

#####################################################################

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Additional Commodities')

Extra.commods                  <- read.csv('extra_commods_barley.csv')

Pastured.cropland              <- read.csv('Pastured Cropland (Census).csv')
Pastured.cropland$Value        <- as.numeric(gsub(',', '', Pastured.cropland$Value))

Pastured.cropland$County.ANSI  <- formatC(Pastured.cropland $County.ANSI, width = 3, format = "d", flag = "0")
Pastured.cropland$County.ANSI  <- assign_other_county(Pastured.cropland )
Pastured.cropland$OverallANSI  <- paste0(Pastured.cropland $State.ANSI, Pastured.cropland $County.ANSI)
Pastured.cropland$USDAagregion <- map_agregion(Pastured.cropland )

Land.county.annual             <- data.frame(County = rep(unique(Pastured.cropland$OverallANSI), times = length(c(1990:2017))),
                                            Year = rep(1990:2017, each  = length(unique(Pastured.cropland$OverallANSI))), 
                                            Value = NA) 

Pastured.cropland.annual       <- merge.data.frame(Land.county.annual, Pastured.cropland, by.x = c('County', 'Year'),
                                                       by.y = c('OverallANSI', 'Year'), all.x = TRUE)   


Pastured.cropland.annual            <- Pastured.cropland.annual %>% dplyr::select(c(1, 2, 8, 22, 24))

Pastured.cropland.annual            <- Pastured.cropland.annual %>% filter(!State %in% c('ALASKA', 'HAWAII'))
PCA.wide                            <- spread(Pastured.cropland.annual[, c(1, 2, 4)], key = County, value = Value.y)

missing                             <- apply(PCA.wide, 2, function(x) {length(which(is.na(x)))})
PCA.wide                            <- data.frame(PCA.wide[, -c(which(missing == 28))])


target.rows <- c(8, 13, 18, 23, 28)

for (i in 1:length(target.rows)) {
  
  for(j in 1:ncol(PCA.wide)) {
    
    if(is.na(PCA.wide[target.rows[i], j])) {
      
      PCA.wide[target.rows[i], j] <- imputeTS::na.locf(PCA.wide[, j])[target.rows[i]]
      
      
    }
    
  }
  
  for(j in 1:ncol(PCA.wide)) {
    
    if(is.na(PCA.wide[target.rows[i], j])) {
      
      PCA.wide[target.rows[i], j] <- imputeTS::na.locf(PCA.wide[, j], option = 'nocb')[target.rows[i]] 
      
      
    }
    
  }
  
  print(i)
  
}

PCA.wide[1:8, 2:ncol(PCA.wide)]     <- apply(PCA.wide[1:8, 2:ncol(PCA.wide)], 2, 
                                                             function(x) {rep(x[8], times = 8)})



PCA.wide            <- data.frame(apply(PCA.wide, 2, 
                                                function(x) {na.interpolation(x, option = 'linear')}))

colnames(PCA.wide)  <- gsub('X', '', colnames(PCA.wide))


PCA        <- gather(PCA.wide[, 2:ncol(PCA.wide)])
PCA$Year   <- c(1990:2017)
colnames(PCA) <- c('OverallANSI', 'Pastured_crop', 'Year')

PCA.test <- merge(PCA, CRP.long, on = c('OverallANSI', 'Year'), all.x = TRUE, all.y = TRUE)
PCA      <- data.frame(PCA.test %>% filter(Year > 1989))

PCA.test <- merge(PCA, CRP[, 1:3], on = 'OverallANSI')

#setwd('E:/Modules/Dissertation/Data/Model Ready/Commodity Footprint')
#
#write.csv(PCA.test, 'CRP_Pasturedcrop.csv', row.names = FALSE)



  ### f) Crop totals

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Additional Commodities')

Idle_cropland              <- read.csv('Idle cropland (Census).csv')
Pastured_cropland          <- read.csv('Pastured Cropland (Census).csv')
Total_cropland             <- read.csv('Total Cropland (Census).csv')
Total_pasture              <- read.csv('Total Pasture (Census).csv')

#Total_cropland$Value       <- as.numeric(gsub(',', '', Total_cropland$Value))
#Total_pasture$Value       <- as.numeric(Total_pasture$Value)
#Pastured_cropland        <- as.numeric(Pastured_cropland$Value)
#Idle_cropland$Value       <- as.numeric(Idle_cropland$Value)

land_stocks                <- list(Idle_cropland, Pastured_cropland, Total_cropland, Total_pasture)
land_stocks                <- rbind.fill(land_stocks)

land_stocks$Value2         <- as.numeric(gsub(',', '', as.character(land_stocks$Value)))
#land_stocks$Value          <- as.numeric(land_stocks$Value)

land_stocks$County.ANSI  <- formatC(land_stocks$County.ANSI, width = 3, format = "d", flag = "0")
land_stocks$County.ANSI  <- assign_other_county(land_stocks)
land_stocks$OverallANSI  <- paste0(land_stocks$State.ANSI, land_stocks$County.ANSI)
land_stocks$USDAagregion <- map_agregion(land_stocks)

teg <- spread(land_stocks, key = Data.Item, value = Value2)
#teg2 <- gather(CRP[, 4:ncol(CRP)])
#teg2$ID <- CRP$ID
#teg2$State <- CRP$State
#teg2$County <- CRP$County
#teg2        <- teg2[1:99836, ]

#teg <- teg[, -c(4, 12:15, 17, 18)]
#teg <- teg[, -c(12)]

#colnames(teg2)[3] <- 'OverallANSI'
#colnames(teg2)[1] <- 'Year'

  ### xxxxx) xxxxxx

Total_pasture$Value        <- as.numeric(Total_pasture$Value)
Total_pasture.State        <- Total_pasture %>% group_by(.dots = c('Year', 'State')) %>% summarise(pasture = sum(Value, na.rm = TRUE))
ggplot(Total_pasture.State, aes(x = Year, y = pasture, colour = State)) + geom_line()
Total_pasture.State               <- filter(Total_pasture.State, State %in% c('OHIO', 'INDIANA', 'ILLINOIS', 'IOWA', 'MISSOURI', 'NORTH DAOKTA', 'SOUTH DAKOTA', 'NEBRASKA', 'KANSAS', 'MISSISSIPPI', 'ARKANSAS', 'LOUISIANA'))

Total_cropland$Value        <- as.numeric(Total_cropland$Value)
Total_cropland.State        <- Total_cropland %>% group_by(.dots = c('Year', 'State')) %>% summarise(crop = sum(Value, na.rm = TRUE))
ggplot(Total_cropland.State, aes(x = Year, y = crop, colour = State)) + geom_line()
Total_cropland.State              <- filter(Total_cropland.State, State %in% c('OHIO', 'IOWA', 
                                                                               'ILLINOIS',  
                                                                               'INDIANA', 'MISSOURI'))

### add total land stocks

setwd('E:/Modules/Dissertation/Data/Processed Dat')

Land.stocks <- read.csv('Land_stocks_Processed.csv')
Land.stocks <- Land.stocks[, -c(1)]



##################################################################################

### 5) Imports

##################################################################################

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Imports and Exports')

import.files              <- list.files(pattern = '*.csv')
import.files              <- import.files[contains('Import', vars = import.files)]



##################################################################################

### 6) Intensification data

##################################################################################

########################## THIS PIECE OF THE SCRIPT TAKES STATEWIDE FERTILISER DATA AND DISAGGREGATES 

  ### 6.1) Corn / Nitrogen


### fillna

fill_na                      <- colwise(function(x) {ifelse(is.na(x), 0, x)})

### unfillna

unfill_na                    <- colwise(function(x) ifelse(x == 0, NA, x))

### no negs

no_negs                      <- colwise(function(x) ifelse(x < 0, 0, x))

setwd('E:/Modules/Dissertation/Data/Processed Dat')

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


#### Import and Clean NA values

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Intensification/Fertilisers by State ERS')

Nitrogen.Corn            <- read.csv('Nitrogen_Corn.csv', skip = 2)
Nitrogen.Corn            <- filter(Nitrogen.Corn, State != '')
Nitrogen.Corn            <- Nitrogen.Corn[-c(29:31), ]    
Nitrogen.Corn            <- dplyr::select(as.tbl(Nitrogen.Corn), c(1, 28:54))
Nitrogen.Corn            <- Nitrogen.Corn[!(rowSums(is.na(Nitrogen.Corn[, 2:28]))==ncol(Nitrogen.Corn[, 2:28])),]       

states                   <- Nitrogen.Corn$State

### Fillna

Nitrogen.Corn            <- data.frame(t(Nitrogen.Corn))      
colnames(Nitrogen.Corn)  <- states
Nitrogen.Corn            <- Nitrogen.Corn[-c(1), ]
Nitrogen.Corn            <- Nitrogen.Corn[, -c(2, 23)]
Nitrogen.Corn            <- na.approx(Nitrogen.Corn)
Nitrogen.Corn            <- data.frame(Nitrogen.Corn)

### Linear models to fill final NA values

correct                  <- colwise(function(x) {as.numeric(as.character(x))})
Nitrogen.Corn            <- correct(Nitrogen.Corn)
Nitrogen.Corn$Year       <- c(1990:2016)

test <- Nitrogen.Corn

for (i in 1:ncol(Nitrogen.Corn)) {
  
  teg                <- lm(as.numeric(as.character(Nitrogen.Corn[, i])) ~ 
                             as.numeric(
    as.character(Nitrogen.Corn$U.S..average.1.)) + as.numeric(
      as.character(Nitrogen.Corn$Iowa)) + as.numeric(
        as.character(Nitrogen.Corn$Nebraska)) + Nitrogen.Corn$Michigan)
  
  teg2               <- predict.lm(teg, newdata = Nitrogen.Corn)
  
  Nitrogen.Corn[, i][is.na(Nitrogen.Corn[, i])] <- teg2[is.na(Nitrogen.Corn[, i])]
  
  
}

Nitrogen.Corn[, 2][is.na(Nitrogen.Corn[, 2])] <- teg2[is.na(Nitrogen.Corn[, 2])]

Corn$Nitrogen.intense <- 1


### Combine with processed Corn frame

for (i in 1:nrow(Corn)) {
  
  try(
  Corn$Nitrogen.intense[i] <- Nitrogen.Corn[which(Nitrogen.Corn$Year == Corn$Year[i]),which(toupper(colnames(Nitrogen.Corn)) == Corn$State[i]) ]
  )
  
  print(i)
}

Corn$Nitrogen.intense     <- ifelse(Corn$Nitrogen.intense == 1, NA, Corn$Nitrogen.intense)

####################################################################################################################

  #### 6.2 Phosphate and Corn


P.Corn            <- read.csv('Phosphate_Corn.csv', skip = 2)
P.Corn            <- filter(P.Corn, State != '')
P.Corn            <- P.Corn[-c(29:31), ]    
P.Corn            <- dplyr::select(as.tbl(P.Corn), c(1, 28:54))
P.Corn            <- P.Corn[!(rowSums(is.na(P.Corn[, 2:28]))==ncol(P.Corn[, 2:28])),]       

states                   <- P.Corn$State

### Fillna

P.Corn            <- data.frame(t(P.Corn))      
colnames(P.Corn)  <- states
P.Corn            <- P.Corn[-c(1), ]
P.Corn            <- P.Corn[, -c(2, 23)]
P.Corn            <- na.approx(P.Corn)
P.Corn            <- data.frame(P.Corn)

### Linear models to fill final NA values

correct                  <- colwise(function(x) {as.numeric(as.character(x))})
P.Corn            <- correct(P.Corn)
P.Corn$Year       <- c(1990:2016)

test <- P.Corn

for (i in 1:ncol(P.Corn)) {
  
  teg                <- lm(as.numeric(as.character(P.Corn[, i])) ~ 
                             as.numeric(
                               as.character(P.Corn$U.S..average.1.)) + as.numeric(
                                 as.character(P.Corn$Iowa)) + as.numeric(
                                   as.character(P.Corn$Nebraska)) + P.Corn$Michigan)
  
  teg2               <- predict.lm(teg, newdata = P.Corn)
  
  P.Corn[, i][is.na(P.Corn[, i])] <- teg2[is.na(P.Corn[, i])]
  
  
}

Corn$Phosphate.intense <- 1


### Combine with processed Corn frame

for (i in 1:nrow(Corn)) {
  
  try(
    Corn$Phosphate.intense[i] <- P.Corn[which(P.Corn$Year == Corn$Year[i]),which(toupper(colnames(P.Corn)) == Corn$State[i]) ]
  )
  
  print(i)
}

Corn$Phosphate.intense     <- ifelse(Corn$Phosphate.intense== 1, NA, Corn$Phosphate.intense)




####################################################################################################################

#### 6.3 Nitrogen and Soy / Phosphate

setwd('E:/Modules/Dissertation/Data/Processed Dat')

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


#### Import and Clean NA values

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Intensification/Fertilisers by State ERS')


N.Soy            <- read.csv('Potash_Soy_Proportion.csv', skip = 2)
N.Soy            <- filter(N.Soy, State != '')
N.Soy            <- N.Soy[-c(27:29), ]    
N.Soy            <- dplyr::select(as.tbl(N.Soy), c(1, 28:53))
N.Soy            <- N.Soy[!(rowSums(is.na(N.Soy[, 2:27]))==ncol(N.Soy[, 2:27])),]       

states                   <- N.Soy$State

### Fillna

N.Soy            <- data.frame(t(N.Soy))      
colnames(N.Soy)  <- states
N.Soy            <- N.Soy[-c(1), ]
N.Soy            <- N.Soy[, -c(2, 10)]
N.Soy            <- na.approx(N.Soy)
N.Soy            <- data.frame(N.Soy)

### Linear models to fill final NA values

correct           <- colwise(function(x) {as.numeric(as.character(x))})
N.Soy             <- correct(N.Soy)
N.Soy$Year        <- c(1990:2015)

test <- N.Soy

for (i in 1:ncol(N.Soy)) {
  
  teg                <- lm(as.numeric(as.character(N.Soy[, i])) ~ 
                             as.numeric(
                               as.character(N.Soy$U.S..average.1.)) + as.numeric(
                                 as.character(N.Soy$Iowa)) + as.numeric(
                                   as.character(N.Soy$Nebraska)) + N.Soy$Michigan)
  
  teg2               <- predict.lm(teg, newdata = N.Soy)
  
  N.Soy[, i][is.na(N.Soy[, i])] <- teg2[is.na(N.Soy[, i])]
  
  
}

Soy$Potash.pct <- 1


### Combine with processed Corn frame

for (i in 1:nrow(Soy)) {
  
  try(
    Soy$Potash.pct[i] <- N.Soy[which(N.Soy$Year == Soy$Year[i]),which(toupper(colnames(N.Soy)) == Soy$State[i]) ]
  )
  
  print(i)
}

Soy$Potash.pct     <- ifelse(Soy$Potash.pct == 1, NA, Soy$Potash.pct)



### Exploratory plot

Corn.agregion <- Soy %>% group_by(.dots = c('Year', 'USDAagregion')) %>% summarise(fertiliser = mean(Potash.pct, na.rm = TRUE), yield = mean(Yield, na.rm = TRUE))

Corn.agregion.long              <- gather(Corn.agregion[, 3:4])
Corn.agregion.long$Year         <- Corn.agregion$Year
Corn.agregion.long$Usdaagregion <- Corn.agregion$USDAagregion

ggplot(filter(Corn.agregion.long, Usdaagregion %in% c(1, 2, 3, 4)), aes(x = Year, y = value, colour = key)) + 
  geom_line() + scale_y_continuous(limits = c(0, 50)) + facet_grid(. ~ Usdaagregion)



##################################################################################

### 6.4) Intensification data

##################################################################################


correct_val                         <- colwise(function(x) {as.numeric(gsub(',', '', as.character(x)))})

setwd('E:/Dissertation/Data/Other/Intensification')

intense.files                       <- list.files(pattern = '*.csv')
intense.files                       <- intense.files[grep('Machinery|Tractors|Chemicals|Seeds', intense.files)]


intense.frame                       <- lapply(intense.files, read.csv)
intense.frame.collapse              <- rbind.fill(intense.frame)
intense.frame.vars                  <- spread(intense.frame.collapse, Data.Item, Value)
colnames(intense.frame.vars)[20:27] <- c('Chemicals_Dollar', 'Chemicals_Pct', 'Machinery_Dollar', 
                                         'Machinery_Pct', 'Seeds_Dollar', 'Seeds_Pct', 
                                         'Tractors_Dollar', 'Tractors_Pct')

intense.frame.vars[, c(20:27)]      <- correct_val(intense.frame.vars[, c(20:27)])

intense.frame.vars                  <- intense.frame.vars %>% group_by(State, Year) %>%
                                      summarise(Chemicals_Dollar = sum(Chemicals_Dollar,na.rm = TRUE), 
                                                Chemicals_Pct = sum(Chemicals_Pct, na.rm = TRUE), 
                                                Machinery_Dollar = sum(Machinery_Dollar, na.rm = TRUE), 
                                                Machinery_Pct = sum(Machinery_Pct, na.rm = TRUE),
                                                Seeds_Dollar  = sum(Seeds_Dollar, na.rm = TRUE), 
                                                Seeds_Pct = sum(Seeds_Pct, na.rm = TRUE), 
                                                Tractors_Dollar = sum(Tractors_Dollar, na.rm = TRUE), 
                                                Tractors_Pct = sum(Tractors_Pct, na.rm = TRUE))


intense.frame.vars.national         <- intense.frame.vars %>% group_by(Year) %>%
  summarise(Chemicals_Dollar = sum(Chemicals_Dollar,na.rm = TRUE), 
            Chemicals_Pct = mean(Chemicals_Pct, na.rm = TRUE), 
            Machinery_Dollar = sum(Machinery_Dollar, na.rm = TRUE), 
            Machinery_Pct = mean(Machinery_Pct, na.rm = TRUE),
            Seeds_Dollar  = sum(Seeds_Dollar, na.rm = TRUE), 
            Seeds_Pct = mean(Seeds_Pct, na.rm = TRUE), 
            Tractors_Dollar = sum(Tractors_Dollar, na.rm = TRUE), 
            Tractors_Pct = mean(Tractors_Pct, na.rm = TRUE))

intense.plot      <- gather(intense.frame.vars.national[, 2:9])
intense.plot$Year <- c(2003:2017)

ggplot(intense.plot, aes(x = Year, y = value)) + geom_line() + facet_grid(. ~ key) +
  scale_y_continuous(limits = c(0, 10))

### seeds % grows significantly, chemicals, machinery and tractors either static or decreasing



##################################################################################

### 6.4b) Merge with Corn and Soy

##################################################################################


setwd('E:/Modules/Dissertation/Data/Intensification/Fertiliser and GMO')
Corn <- read.csv('Corn_Fertiliser_GMO.csv')
Soy  <- read.csv('Soy_Fertiliser_GMO.csv')

Corn$Chemicals_Dollar     <- NA
Corn$Machinery_Dollar     <- NA
Corn$Seeds_Dollar         <- NA


for (i in 28012:nrow(Corn)) {
  
  try(
    Corn$Chemicals_Dollar[i] <- intense.frame.vars[which(intense.frame.vars$Year == Corn$Year[i]& toupper(intense.frame.vars$State) == Corn$State[i]),]$Chemicals_Dollar
  )
  
  print(i)
}

for (i in 20812:nrow(Corn)) {
  
  try(
    Corn$Machinery_Dollar[i] <- intense.frame.vars[which(intense.frame.vars$Year == Corn$Year[i]& toupper(intense.frame.vars$State) == Corn$State[i]),]$Machinery_Dollar
  )
  
  print(i)
}

for (i in 20812:nrow(Corn)) {
  
  try(
    Corn$Seeds_Dollar[i] <- intense.frame.vars[which(intense.frame.vars$Year == Corn$Year[i]& toupper(intense.frame.vars$State) == Corn$State[i]),]$Seeds_Dollar
  )
  
  print(i)
}


  ### do Soy

Soy$Chemicals_Dollar     <- NA
Soy$Machinery_Dollar     <- NA
Soy$Seeds_Dollar         <- NA


for (i in 21501:nrow(Soy)) {
  
  try(
    Soy$Chemicals_Dollar[i] <- intense.frame.vars[which(intense.frame.vars$Year == Soy$Year[i]& toupper(intense.frame.vars$State) == Soy$State[i]),]$Chemicals_Dollar
  )
  
  print(i)
}

for (i in 21501:nrow(Soy)) {
  
  try(
    Soy$Machinery_Dollar[i] <- intense.frame.vars[which(intense.frame.vars$Year == Soy$Year[i]& toupper(intense.frame.vars$State) == Soy$State[i]),]$Machinery_Dollar
  )
  
  print(i)
}

for (i in 21501:nrow(Soy)) {
  
  try(
    Soy$Seeds_Dollar[i] <- intense.frame.vars[which(intense.frame.vars$Year == Soy$Year[i]& toupper(intense.frame.vars$State) == Soy$State[i]),]$Seeds_Dollar
  )
  
  print(i)
}



##################################################################################

### 6.4c) Merge annual intense data with prior census years

##################################################################################

### Corn

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Corn/Profit Loss Model')

profit.loss.files <- list.files(pattern = '*.csv')
profit.loss       <- lapply(profit.loss.files[c(4:8)], read.csv, skip = 1)
profit.loss       <- lapply(profit.loss, function(x) {x %>% filter(nchar(as.character(X)) > 3)})

Chemicals.Seed    <- lapply(profit.loss, function(x) {x %>% dplyr::filter(X %in% c('Seed', 'Chemicals'))})

for(i in 4:8) {
  
  Chemicals.Seed[[i-3]]$Region <- profit.loss.files[i]
  Chemicals.Seed[[i-3]]$Region <- gsub('.csv', '', Chemicals.Seed[[i-3]]$Region)
  
}

Chemicals.Seed             <- rbind.fill(Chemicals.Seed) 
Chemicals.Seed.Long        <- gather(Chemicals.Seed[, c(2:23)])
Chemicals.Seed.Long$Region <- Chemicals.Seed$Region
Chemicals.Seed.Long$Var    <- Chemicals.Seed$X
Chemicals.Seed.Long$Year   <- as.numeric(substr(Chemicals.Seed.Long$key, 2, nchar(Chemicals.Seed.Long$key)))
Chemicals.Seed.Long        <- Chemicals.Seed.Long %>% dplyr::select (c(2:ncol(Chemicals.Seed.Long)))

Chemicals.Seed.Long$Region.int <- c(1, 1,2, 2, 3, 3, 4, 4, 6, 6)

#### Merge Chemicals.Seed.Long with existing intensification data in Corn & repeat for Soy


##################################################################################

### 6.5) GMO

##################################################################################

correct_val                          <- colwise(function(x) {as.character(gsub('.', 'teg', as.character(x)))})
correct_val2                         <- colwise(function(x) {as.numeric(gsub('*', '', as.character(x)))})

setwd('E:/Modules/Dissertation/Data/Intensification/Fertiliser by Commod_Processed')

GMO                 <- read.csv('GMO_commod.csv')
GMO                 <- GMO[, -1]
GMO.commod          <- spread(GMO, Crop.title, Value)
GMO.commod[, c(6:8)]<- correct_val2(GMO.commod[, c(6:8)])


GMO.commod    <- GMO.commod %>% group_by(.dots = c(colnames(GMO.commod[c(1, 3:4)]))) %>%
                  summarise(Corn = sum(`Genetically engineered (GE) corn varieties`, na.rm = TRUE), 
                            Soy = sum(`Genetically engineered (GE) soybean varieties`, na.rm = TRUE), 
                            Cotton = sum(`Genetically engineered (GE) upland cotton varieties`, na.rm = TRUE))


### correct to group_by Variety

GMO.commod$Variety   <- as.character(GMO.commod$Variety)

GMO.commod$Variety   <- ifelse(GMO.commod$Variety == unique(GMO.commod$Variety)[5], 
                               unique(GMO.commod$Variety)[1], GMO.commod$Variety)

GMO.commod$Variety   <- ifelse(GMO.commod$Variety == unique(GMO.commod$Variety)[2], 
                               unique(GMO.commod$Variety)[5], GMO.commod$Variety)

GMO.commod$Variety   <- ifelse(GMO.commod$Variety == unique(GMO.commod$Variety)[4], 
                               unique(GMO.commod$Variety)[5], GMO.commod$Variety)

GMO.commod$Variety   <- gsub('                                           ', '', GMO.commod$Variety)

### delete blank rows

GMO.commod    <- GMO.commod %>% group_by(State, Variety, Year) %>%
  summarise(Corn = sum(Corn, na.rm = TRUE), 
            Soy = sum(Soy, na.rm = TRUE), 
            Cotton = sum(Cotton, na.rm = TRUE))

### Split by commod to predict backwards to 1996

GMO.commod.Soy       <- spread(GMO.commod, Variety, Soy)

GMO.commod.Soy       <- GMO.commod.Soy %>% group_by(Year, State) %>%
                          summarise(Soy_all = sum(`All GE varieties`, na.rm = TRUE), 
                                    Soy_Herbicide = sum(`Herbicide-tolerant only`, na.rm = TRUE), 
                                    Soy_Insecticide = sum(`Insect-resistant (Bt) only`, na.rm = TRUE), 
                                    Soy_stacked = sum(`Stacked gene varieties`, na.rm = TRUE))


GMO.commod.Corn      <- spread(GMO.commod, Variety, Corn)

GMO.commod.Corn       <- GMO.commod.Corn %>% group_by(Year, State) %>%
  summarise(Corn_all = sum(`All GE varieties`, na.rm = TRUE), 
            Corn_Herbicide = sum(`Herbicide-tolerant only`, na.rm = TRUE), 
            Corn_Insecticide = sum(`Insect-resistant (Bt) only`, na.rm = TRUE), 
            Corn_stacked = sum(`Stacked gene varieties`, na.rm = TRUE))

GMO.commod.Cotton    <- spread(GMO.commod, Variety, Cotton)

GMO.commod.Cotton       <- GMO.commod.Cotton %>% group_by(Year, State) %>%
  summarise(Cotton_all = sum(`All GE varieties`, na.rm = TRUE), 
            Cotton_Herbicide = sum(`Herbicide-tolerant only`, na.rm = TRUE), 
            Cotton_Insecticide = sum(`Insect-resistant (Bt) only`, na.rm = TRUE), 
            Cotton_stacked = sum(`Stacked gene varieties`, na.rm = TRUE))


################################################################################################

### 6.5b) extrapolate back to 1996

################################################################################################

baxtrapolate                  <- function(x, y) {
  
  t <- list()
  
  for (i in 1:length(levels(factor(x$State)))) {
    
    t[[i]] <- data.frame(filter(x, State == levels(factor(x$State))[i]))
    
  }
  
  ### add empty years back to 1996 then extrapolate
  
  s         <- lapply(t, function(x) {rbind(x, c(1996, levels(factor(x$State)[i]), 0, 0, 0, 0))})
  s         <- lapply(s, function(x) {rbind(x, c(1997, levels(factor(x$State)[i]), NA, NA, NA, NA))})
  s         <- lapply(s, function(x) {rbind(x, c(1998, levels(factor(x$State)[i]), NA, NA, NA, NA))})
  s         <- lapply(s, function(x) {rbind(x, c(1999, levels(factor(x$State)[i]), NA, NA, NA, NA))})

  for(i in 1:length(s)) {
  
    for(j in 3:6) {
    
      s[[i]][s[[i]]$Year == 1999, j] <- 0.75*as.numeric(s[[i]][s[[i]]$Year == 2000, j])
      s[[i]][s[[i]]$Year == 1998, j] <- 0.5*as.numeric(s[[i]][s[[i]]$Year == 2000, j])
      s[[i]][s[[i]]$Year == 1997, j] <- 0.25*as.numeric(s[[i]][s[[i]]$Year == 2000, j])

    }
  }
  
  s <- rbind.fill(s)
  
  return(s)
  
}  


GMO.commod.Corn   <- baxtrapolate(GMO.commod.Corn)
GMO.commod.Soy    <- baxtrapolate(GMO.commod.Soy)
GMO.commod.Cotton <- baxtrapolate(GMO.commod.Cotton)

GMO.commod        <- base::merge(GMO.commod.Corn, GMO.commod.Soy)
GMO.commod        <- base::merge(GMO.commod, GMO.commod.Cotton)


################################################################################################

### 6.5c) Merge with Soy and Corn frames

################################################################################################

setwd('E:/Modules/Dissertation/Data/Intensification/Fertiliser by Commod_Processed')

GMO.commod <- read.csv('GMO_commod.csv')


setwd('E:/Modules/Dissertation/Data/Intensification/Fertiliser and GMO')
Corn <- read.csv('Corn_Fertiliser_GMO.csv')
Soy  <- read.csv('Soy_Fertiliser_GMO.csv')


Corn$GM_Herbicide   <- 0
Corn$GM_Insecticide <- 0
Corn$GM_Stacked     <- 0


for (i in 1:nrow(Corn)) {
  
  try(
    Corn$GM_Herbicide[i] <- GMO.commod[which(GMO.commod$Year == Corn$Year[i]& toupper(GMO.commod$State) == Corn$State[i]),]$Corn_Herbicide
  )
  
  print(i)
}

for (i in 1:nrow(Corn)) {
  
  try(
    Corn$GM_Insecticide[i] <- GMO.commod[which(GMO.commod$Year == Corn$Year[i]& toupper(GMO.commod$State) == Corn$State[i]),]$Corn_Insecticide
  )
  
  print(i)
}

for (i in 1:nrow(Corn)) {
  
  try(
    Corn$GM_Stacked[i] <- GMO.commod[which(GMO.commod$Year == Corn$Year[i]& toupper(GMO.commod$State) == Corn$State[i]),]$Corn_Stacked
  )
  
  print(i)
}

Corn$GM_total <- Corn$GM + Corn$GM_Herbicide + Corn$GM_Insecticide
Corn$GM_Stacked <- Corn$GM
Corn <- Corn[, -(31)]


### exploratory plots

Soy.agregion <- Corn %>% group_by(Year, USDAagregion) %>% summarise(GM_stacked = mean(GM_total, na.rm = TRUE), Yield = mean(Yield, na.rm = TRUE))

ggplot(filter(Soy.agregion, USDAagregion %in% c(1,2,3,4) & GM_stacked != 0), aes(x = GM_stacked, y = Yield, colour = factor(USDAagregion))) +
  geom_point() + geom_smooth(se = FALSE)



setwd('E:/Modules/Dissertation/Data/Intensification/Fertiliser and GMO')

#write.csv(Corn, 'Corn_Fertiliser_GMO.csv')


###############################################################################################

### 6.zzzzzz Try using AGregion intensification data in stead of statewide survey

###############################################################################################

setwd('E:/Modules/Dissertation/Data/Model Ready')

Corn.intense <- read.csv('Corn_subsidies.csv')
Soy.intense  <- read.csv('Soy_subsidies.csv')


setwd('E:/Modules/Dissertation/Data/Model Ready/Economic model')

Corn.economic <- read.csv('Corn_economic.csv')
Soy.economic  <- read.csv('Soy_economic.csv')


####################################################################

### Merge in by USDA data

####################################################################

Corn.intense  <- merge(Corn.intense, Corn.economic[, c(1, 2, 16, 17, 18)], all.x = TRUE)
Soy.intense   <- merge(Soy.intense, Soy.economic[, c(1, 2, 13:15)], all.x = TRUE)

setwd('E:/Modules/Dissertation/Data/Model Ready')

#write.csv(Corn.intense, 'Corn_intense_USDA.csv', row.names = FALSE)
#write.csv(Soy.intense, 'Soy_intense_USDA.csv', row.names = FALSE)


##################################################################################################################

### 7) Merge climate data with Soy & Corn

##################################################################################################################

setwd('E:/Modules/Dissertation/Data/Climate/Final')

Prec <- read.csv('precipitation_annual.csv')
Temp <- read.csv('tempave_annual.csv')

Corn.climate        <- Corn[, -c(1)]
Corn.climate$prec   <- NA
Corn.climate$temperature <- NA

for (i in 1:nrow(Corn.climate)) {

  try(
      Corn.climate$prec[i]   <- Prec[which(Prec$Year == Corn.climate$Year[i] & Prec$OverallANSI == Corn.climate$OverallANSI[i]), 3]
    )
  print(i)
}

for (i in 1:nrow(Corn.climate)) {
  
  try(
    Corn.climate$temperature[i]   <- Temp[which(Temp$Year == Corn.climate$Year[i] & Temp$OverallANSI == Corn.climate$OverallANSI[i]), 3]
  )
  print(i)
}

setwd('E:/Modules/Dissertation/Data/Model Ready')

write.csv(Corn.climate, 'Corn3_climate.csv')

### Soy

setwd('E:/Modules/Dissertation/Data/Intensification/Intense_GMO_Fertiliser')

Soy <- read.csv('Soy3_CRP.csv')

Soy.climate             <- Soy[, -c(3:5)]
Soy.climate$prec        <- NA
Soy.climate$temperature <- NA

for (i in 1:nrow(Soy.climate)) {
  
  try(
    Soy.climate$prec[i]   <- Prec[which(Prec$Year == Soy.climate$Year[i] & Prec$OverallANSI == Soy.climate$OverallANSI[i]), 3]
  )
  print(i)
}

for (i in 1:nrow(Soy.climate)) {
  
  try(
    Soy.climate$temperature[i]   <- Temp[which(Temp$Year == Soy.climate$Year[i] & Temp$OverallANSI == Soy.climate$OverallANSI[i]), 3]
  )
  print(i)
}






#######################################################################################################

### 8) Economic models

#######################################################################################################

##### Read in

### Soy

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Soy/Profit Loss Model')

Soy.files       <- list.files(pattern = '*.csv')
Soy.dat         <- lapply(Soy.files, read.csv, skip = 1)
Soy.dat         <- lapply(Soy.dat, function(x) {x[-c(grep('.*?([0-9]+).*', x$X)), ]})
names(Soy.dat)  <- substr(Soy.files, 1, nchar(Soy.files) - 4)

### Select key data

cnames          <- Soy.dat[[1]][, 1]
Soy.dat         <- lapply(Soy.dat, function(x) {data.frame(t(x))})
Soy.dat         <- lapply(Soy.dat, setNames, cnames)
Soy.dat         <- lapply(Soy.dat, function(x) {x[-c(1), ]})
Soy.dat         <- lapply(Soy.dat, function(x) {x[, c(2, 5, 6, 7, 8, 9, 
                                                      10,13, 21, 23,25, 26, 28, 29, 30, 32, 33)]})

### Turn into one data frame

Soy.df         <- rbind.fill(Soy.dat)
Soy.df$Year    <- 1997:2017
Soy.df$Region  <- rep(gsub('\\.csv', '', Soy.files), each = 21)

### Corn

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Corn/Profit Loss Model')

Corn.files        <- list.files(pattern = '*.csv')
Corn.dat          <- lapply(Corn.files, read.csv, skip = 1)
Corn.dat          <- lapply(Corn.dat, function(x) {x[-c(grep('.*?([0-9]+).*', x$X)), ]})
names(Corn.dat)   <- substr(Corn.files, 1, nchar(Corn.files) - 4)

### Select key data

cnames          <- Corn.dat[[1]][, 1]
Corn.dat         <- lapply(Corn.dat, function(x) {data.frame(t(x))})
Corn.dat         <- lapply(Corn.dat, setNames, cnames)
Corn.dat         <- lapply(Corn.dat, function(x) {x[-c(1), ]})
Corn.dat         <- lapply(Corn.dat, function(x) {x[, c(4, 6, 7, 8, 9, 
                                                     10,11,14, 22, 24, 26, 27, 29, 30, 31, 33, 34)]})

### Turn into one data frame

Corn.df         <- rbind.fill(Corn.dat)
Corn.df$Year    <- 1996:2017
Corn.df$Region  <- rep(gsub('\\.csv', '', Corn.files), each = 22)


### Fertiliser data

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Agricultural context')

Fertiliser.dat   <- read.csv('fertilizer_profitloss.csv', skip = 2)
Fertiliser.dat   <- Fertiliser.dat[-c(1:32), ]
Fertiliser.dat   <- Fertiliser.dat[, -c(4)]
Fertiliser.dat   <- Fertiliser.dat[1:27, ]
Fertiliser.dat$Year <- as.integer(as.character(Fertiliser.dat$Year))
Fertiliser.dat[28, ] <- c(2017, rep(NA, times=  6))   
  
Fertiliser.dat$Paid.by.farmers.for.fertilizer <- as.numeric(as.character(
                    Fertiliser.dat$Paid.by.farmers.for.fertilizer))


### US Dollar historical

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Agricultural context')

Dollar        <- read.csv('Dollar_Euro.csv', skip = 3, stringsAsFactors = FALSE)
Dollar$DATE   <- gsub('/', ',', as.character(Dollar$DATE))
Dollar$DATE   <- as.Date(Dollar$DATE, format = '%d,%m,%Y')
Dollar$Year   <- as.integer(substr(Dollar$DATE, 1, 4))

Dollar.annual <- data.frame(Dollar %>% group_by(Year) %>% 
                              summarise(Value = mean(XUDLERD, na.rm = TRUE))) 


### checking production calc vs Soy data

teg <- Soy %>% group_by(.dots = c('USDAagregion', 'Year')) %>% summarise(Yield = mean(Yield, na.rm = TRUE))



#######################################################################################################

### 8b) Extrapolate data back to 1990

#######################################################################################################


Corn.baxtrapolate <- Corn.dat

while(nrow(Corn.baxtrapolate[[1]]) < 28){

Corn.baxtrapolate <- lapply(Corn.baxtrapolate, function(x) {
  
  add_row(x)
})

}

Corn.baxtrapolate <- lapply(Corn.baxtrapolate, function(x) {
  
  x[c(23:28, 22:1), ]
  
})


### Turn into one data frame

Corn.df         <- rbind.fill(Corn.baxtrapolate)

Corn.df         <- data.frame(apply(Corn.df, 2, function(x) {
  
  as.numeric(x)
  
}))

Corn.df$Year    <- 1990:2017
Corn.df$Region  <- rep(gsub('\\.csv', '', Corn.files), each = 28)



Soy.baxtrapolate <- Soy.dat

while(nrow(Soy.baxtrapolate[[1]]) < 28){
  
  Soy.baxtrapolate <- lapply(Soy.baxtrapolate, function(x) {
    
    add_row(x)
  })
  
}

Soy.baxtrapolate <- lapply(Soy.baxtrapolate, function(x) {
  
  x[c(22:28, 21:1), ]
  
})


### Turn into one data frame

Soy.df         <- rbind.fill(Soy.baxtrapolate)
Soy.df         <- data.frame(apply(Soy.df, 2, function(x) {
  
  as.numeric(x)
  
}))

Soy.df$Year    <- rep(1990:2017, times= 7)
Soy.df$Region  <- rep(gsub('\\.csv', '', Soy.files), each = 28)


########################################################################

### Add yield data by agregion

########################################################################

setwd('E:/Modules/Dissertation/Data/Model Ready')

Soy  <- read.csv('Soy_subsidies.csv')
Corn <- read.csv('Corn_subsidies.csv')


Soy.agregion.Yield       <- Soy %>% group_by(.dots = c('Year', 'USDAagregion')) %>%
                        summarise(Produced = sum(SOYBEANS...PRODUCTION..MEASURED.IN.BU, na.rm =TRUE), 
                                  Harvested = sum(SOYBEANS...ACRES.HARVESTED, na.rm = TRUE), 
                                  Price = mean(SOYBEANS...PRICE.RECEIVED..MEASURED.IN.....BU, na.rm = TRUE))

Soy.agregion.Yield       <- data.frame(Soy.agregion.Yield)

Soy.agregion.Yield$Yield <- Soy.agregion.Yield$Produced / Soy.agregion.Yield$Harvested 


Corn.agregion.Yield <- Corn %>% group_by(.dots = c('Year', 'USDAagregion')) %>%
  summarise(Produced = sum(CORN..GRAIN...PRODUCTION..MEASURED.IN.BU, na.rm =TRUE), 
            Harvested = sum(CORN..GRAIN...ACRES.HARVESTED, na.rm = TRUE), 
            Price = mean(CORN..GRAIN...PRICE.RECEIVED..MEASURED.IN.....BU, na.rm = TRUE))

Corn.agregion.Yield <- data.frame(Corn.agregion.Yield) 

Corn.agregion.Yield$Yield <- Corn.agregion.Yield$Produced / Corn.agregion.Yield$Harvested 


Soy.agregion.Yield <- data.frame(Soy.agregion.Yield %>% 
  mutate(Agregion.name = ifelse(USDAagregion == 1, 'Heartland',
          ifelse(USDAagregion == 2, 'Northern Crescent',
                ifelse(USDAagregion == 3, 'Northern Great Plains',
                     ifelse(USDAagregion == 4, 'Prarie Gateway', 
                            ifelse(USDAagregion == 5, 'Eastern Uplands', 
                                   ifelse(USDAagregion == 6, 'Southern Seaboard',
                                          ifelse(USDAagregion == 7, 'Fruitful Rim', 
                                                 ifelse(USDAagregion == 8, 'Basin and Range', 
                                                        ifelse(USDAagregion == 9, 'Mississipi Portal', 
                                                               ifelse(is.na(USDAagregion), NA, ''))))))))))))

Corn.agregion.Yield <- data.frame(Corn.agregion.Yield %>% 
  mutate(Agregion.name = ifelse(USDAagregion == 1, 'Heartland',
                                ifelse(USDAagregion == 2, 'Northern Crescent',
                                       ifelse(USDAagregion == 3, 'Northern Great Plains',
                                              ifelse(USDAagregion == 4, 'Prarie Gateway', 
                                                     ifelse(USDAagregion == 5, 'Eastern Uplands', 
                                                            ifelse(USDAagregion == 6, 'Southern Seabord',
                                                                   ifelse(USDAagregion == 7, 'Fruitful Rim', 
                                                                          ifelse(USDAagregion == 8, 'Basin and Range', 
                                                                                 ifelse(USDAagregion == 9, 'Mississipi Portal', 
                                                                                        ifelse(is.na(USDAagregion), NA, ''))))))))))))


Soy.agregion.Yield  <- Soy.agregion.Yield[!is.na(Soy.agregion.Yield$USDAagregion), ]
Corn.agregion.Yield <- Corn.agregion.Yield[!is.na(Corn.agregion.Yield$USDAagregion), ]


########################################################################

### Add yield

########################################################################

for(i in 1:nrow(Soy.df)) {

    Soy.df$Yield..bushels.per.planted.acre.[i] <- Soy.agregion.Yield[which(Soy.agregion.Yield$Agregion.name == Soy.df$Region[i] & 
                                                                           Soy.agregion.Yield$Year == Soy.df$Year[i]), 6]

    print(i)


}

for(i in 1:nrow(Corn.df)) {

    try(
    
    Corn.df$Yield..bushels.per.planted.acre.[i] <- as.numeric(Corn.agregion.Yield[which(Corn.agregion.Yield$Agregion.name == Corn.df$Region[i] & 
                                                                               Corn.agregion.Yield$Year == Corn.df$Year[i]), 6])
    )
    
    print(i)

}



########################################################################

### Add price

########################################################################

for(i in 1:nrow(Soy.df)) {
  
  Soy.df$Price..dollars.per.bushel.at.harvest.[i] <- Soy.agregion.Yield[which(Soy.agregion.Yield$Agregion.name == Soy.df$Region[i] & 
                                                                           Soy.agregion.Yield$Year == Soy.df$Year[i]), 5]
  
  print(i)
  
  
}

for(i in 1:nrow(Corn.df)) {
  
  try(
    
    Corn.df$Price..dollars.per.bushel.at.harvest.[i] <- as.numeric(Corn.agregion.Yield[which(Corn.agregion.Yield$Agregion.name == Corn.df$Region[i] & 
                                                                                          Corn.agregion.Yield$Year == Corn.df$Year[i]), 5])
  )
  
  print(i)
  
}


########################################################################

### Substitute Gross value of production

########################################################################

Corn.df$Total..gross.value.of.production <- Corn.df$Price..dollars.per.bushel.at.harvest. * Corn.df$Yield..bushels.per.planted.acre.

Soy.df$Primary.product.soybeans  <- Soy.df$Price..dollars.per.bushel.at.harvest. * Soy.df$Yield..bushels.per.planted.acre.



#######################################################################

### Set seed value < 1997 @ == 1997

#######################################################################



for (i in 1:length(unique(Soy.df$Region))) {
  
  for (j in 1:nrow(Soy.df)) {
    
    if(Soy.df$Year[j] < 1997 & Soy.df$Region[j] == unique(Soy.df$Region)[i]) {
    
    Soy.df$Seed[j] <- Soy.df$Seed[Soy.df$Region == unique(Soy.df$Region)[i] & Soy.df$Year == 1997]
    
    print(i)
  
    }
      
  }
}
  

for (i in 1:length(unique(Corn.df$Region))) {
  
  for (j in 1:nrow(Corn.df)) {
    
    if(Corn.df$Year[j] < 1996 & Corn.df$Region[j] == unique(Corn.df$Region)[i]) {
      
      Corn.df$Seed[j] <- Corn.df$Seed[Corn.df$Region == unique(Corn.df$Region)[i] & Corn.df$Year == 1996]
      
      print(i)
      
    }
    
  }
}


#######################################################################

### Fertiliser values from USDA ERS national values

#######################################################################


Corn.Fertiliser <- list()

#### user Fertiliser.dat not this

for (i in 1:length(unique(Corn.df$Region))) {

  Corn.Fertiliser[[i]] <-   predict.lm(lm(Corn.df$Fertilizer.áµf.[Corn.df$Region == unique(Corn.df$Region)[i]] ~ 
                                            Fertiliser.dat$Paid.by.farmers.for.fertilizer), 
                                       newdata = data.frame(Fertilizer.áµf. = 
                                                              Corn.df$Fertilizer.áµf.[Corn.df$Region == unique(Corn.df$Region)[i]], 
                                                            Paid.by.farmers.for.fertilizer =  Fertiliser.dat$Paid.by.farmers.for.fertilizer))
  

}


Corn.df$Fert.pred                                       <- unlist(Corn.Fertiliser)
Corn.df$Fertilizer.áµf.[is.na(Corn.df$Fertilizer.áµf.)] <- Corn.df$Fert.pred[is.na(Corn.df$Fertilizer.áµf.)] 
Corn.df                                                 <- Corn.df[, -c(which(colnames(Corn.df) == 'Fert.pred'))]  



Soy.fertiliser <- list()

for (i in 1:length(unique(Soy.df$Region))) {
  
  Soy.fertiliser[[i]] <-   predict.lm(lm(Soy.df$Fertilizer.áµf.[Soy.df$Region == unique(Soy.df$Region)[i]] ~ 
                                           Fertiliser.dat$Paid.by.farmers.for.fertilizer), 
                                      newdata = data.frame(Fertilizer.áµf. = 
                                                             Soy.df$Fertilizer.áµf.[Soy.df$Region == unique(Soy.df$Region)[i]], 
                                                           Paid.by.farmers.for.fertilizer =  Fertiliser.dat$Paid.by.farmers.for.fertilizer))
  
  
}


#for (i in 1:length(unique(Soy.df$Region))) {
#  
#  Soy.fertiliser[[i]] <-   summary(lm(Soy.df$Fertilizer.áµf.[Soy.df$Region == unique(Soy.df$Region)[i]] ~ 
#                                           Fertiliser.dat$Paid.by.farmers.for.fertilizer))
#  
#  
#}

Soy.df$Fertiliser.pred <- unlist(Soy.fertiliser)
Soy.df$Fertilizer.áµf.[is.na(Soy.df$Fertilizer.áµf.)] <- Soy.df$Fertiliser.pred[is.na(Soy.df$Fertilizer.áµf.)]
Soy.df           <- Soy.df[, -c(which(colnames(Soy.df) == 'Fertiliser.pred'))]

## *********************************************************************************************

### PICK UP FROM HERE (10/05/2019)

## **********************************************************************************************


#setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Soy/Profit Loss Processed')
#Soy.df    <- read.csv('Soy_profitloss_processed.csv')
#setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Corn/Profit Loss Processed')
#Corn.df   <- read.csv('Corn_profitloss_processed.csv') 


Corn.chemicals <- list()

for (i in 1:length(unique(Corn.df$Region))) {
  
  Corn.chemicals[[i]] <-   predict.lm(lm(Corn.df$Chemicals[
                                Corn.df$Region == unique(Corn.df$Region)[i]] ~
                                  Corn.df$Seed[
                                    Corn.df$Region == unique(Corn.df$Region)[i]]), 
                                newdata = Corn.df[Corn.df$Region == unique(Corn.df$Region)[i], ])
  
  
}

Corn.df$Chemicals.pred3                     <- unlist(Corn.chemicals)
Corn.df$Chemicals[is.na(Corn.df$Chemicals)]    <- Corn.df$Chemicals.pred3[is.na(Corn.df$Chemicals)]


#### Soy

Soy.chemicals <- list()

for (i in 1:length(unique(Soy.df$Region))) {
  
  Soy.chemicals[[i]] <-   predict.lm(lm(Soy.df$Chemicals[
    Soy.df$Region == unique(Soy.df$Region)[i]] ~
      Soy.df$Seed[
        Soy.df$Region == unique(Soy.df$Region)[i]]), 
    newdata = Soy.df[Soy.df$Region == unique(Soy.df$Region)[i], ])
  
  
}

for (i in 1:length(unique(Soy.df$Region))) {
  
  Soy.chemicals[[i]] <-   summary(lm(Soy.df$Chemicals[
    Soy.df$Region == unique(Soy.df$Region)[i]] ~
      Soy.df$Seed[
        Soy.df$Region == unique(Soy.df$Region)[i]]))
  
  
}

Soy.df$Chemicals.pred                          <- unlist(Soy.chemicals)
Soy.df$Chemicals[is.na(Soy.df$Chemicals)]      <- Soy.df$Chemicals.pred[is.na(Soy.df$Chemicals)]

for (i in 1:length(unique(Soy.df$Region))) {
  
  for(j in 1:nrow(Soy.df)) {
    
    if(Soy.df$Year[j] < 1997) {
   
   Soy.df$Chemicals[j] <- Soy.df$Chemicals[which(Soy.df$Region == unique(Soy.df$Region)[i] & Soy.df$Year == 1997)]
  
   print(j)
   
    }
    
  }
   
}

#######################################################################################################

### Fuel and bespoke services

#######################################################################################################

Soy.custom <- list()

for (i in 1:length(unique(Soy.df$Region))) {
  
  Soy.custom[[i]] <-   predict.lm(lm(Soy.df$Custom.services[
    Soy.df$Region == unique(Soy.df$Region)[i]] ~
      Soy.df$Year[
        Soy.df$Region == unique(Soy.df$Region)[i]]), 
    newdata = Soy.df[Soy.df$Region == unique(Soy.df$Region)[i], ])
  
  
}

Soy.df$Cusom.pred      <- unlist(Soy.custom)
Soy.df$Custom.services[is.na(Soy.df$Custom.services)] <- Soy.df$Cusom.pred[is.na(Soy.df$Custom.services)] 

for (i in 1:length(unique(Soy.df$Region))) {
  
  for(j in 1:nrow(Soy.df)) {
    
    if(Soy.df$Year[j] < 1997 & Soy.df$Region[j] == 'Mississipi Portal') {
      
      Soy.df$Custom.services[j] <- Soy.df$Custom.services[which(Soy.df$Region == unique(Soy.df$Region)[i] & Soy.df$Year == 1996)]
      
      print(j)
      
    }
    
  }
  
}

### lube

Soy.lube <- list()

for (i in 1:length(unique(Soy.df$Region))) {
  
  Soy.lube[[i]] <-   predict.lm(lm(Soy.df$Fuel..lube..and.electricity[
    Soy.df$Region == unique(Soy.df$Region)[i]] ~
      Soy.df$Year[
        Soy.df$Region == unique(Soy.df$Region)[i]]), 
    newdata = Soy.df[Soy.df$Region == unique(Soy.df$Region)[i], ])
  
  
}

Soy.df$Lube.pred         <- unlist(Soy.lube)
Soy.df$Fuel..lube..and.electricity[is.na(Soy.df$Fuel..lube..and.electricity)] <- Soy.df$Lube.pred[is.na(Soy.df$Fuel..lube..and.electricity)] 
Soy.df$Fuel..lube..and.electricity[Soy.df$Region == 'Mississipi Portal' & Soy.df$Year < 1997]   <-  Soy.df$Lube.pred[Soy.df$Region == 'Mississipi Portal' & Soy.df$Year == 1996]


### Repairs

Soy.repairs <- list()

for (i in 1:length(unique(Soy.df$Region))) {
  
  Soy.repairs[[i]] <-   predict.lm(lm(Soy.df$Repairs[
    Soy.df$Region == unique(Soy.df$Region)[i]] ~
      Soy.df$Year[
        Soy.df$Region == unique(Soy.df$Region)[i]]), 
    newdata = Soy.df[Soy.df$Region == unique(Soy.df$Region)[i], ])
  
  
}

Soy.df$Repairs.pred         <- unlist(Soy.repairs)
Soy.df$Repairs[is.na(Soy.df$Repairs)] <- Soy.df$Repairs.pred[is.na(Soy.df$Repairs)] 
Soy.df$Repairs[Soy.df$Region == 'Mississipi Portal' & Soy.df$Year < 1995]   <-  Soy.df$Repairs.pred[Soy.df$Region == 'Mississipi Portal' & Soy.df$Year == 1995]
Soy.df$Repairs[Soy.df$Region == 'Northern Great Plains' & Soy.df$Year < 1995]   <-  Soy.df$Repairs.pred[Soy.df$Region == 'Northern Great Plains' & Soy.df$Year == 1995]
Soy.df$Repairs[Soy.df$Region == 'Southern Seaboard' & Soy.df$Year < 1993]   <-  Soy.df$Repairs.pred[Soy.df$Region == 'Southern Seaboard' & Soy.df$Year == 1993]

#write.csv(Soy.df, 'Soy_processed_profitloss2.csv', row.names = FALSE)


#######################################################################################################

### Corn - Fuel and bespoke services

#######################################################################################################

for (i in 1:length(unique(Corn.df$Region))) {
  
  for(j in 1:nrow(Corn.df)) {
    
    if(Corn.df$Year[j] < 1996 & Corn.df$Region[j] == unique(Corn.df$Region)[i]) {
      
      Corn.df$Custom.services.áµ..[j] <- Corn.df$Custom.services.áµ..[which(Corn.df$Region == unique(Corn.df$Region)[i] & Corn.df$Year == 1996)]
      
      print(j)
      
    }
    
  }
  
}


### Repairs

Corn.repairs <- list()

Corn.df.test <- Corn.df

for (i in 1:length(unique(Corn.df$Region))) {
  
  for(j in 1:nrow(Corn.df)) {

    if(Corn.df$Year[j] < 1996 & Corn.df$Region[j] == unique(Corn.df$Region)[i]){
    
      gradient <- (Corn.df$Repairs[Corn.df$Year == 2000 & Corn.df$Region == unique(Corn.df$Region)[i]] - 
        Corn.df$Repairs[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]]) / 4
    
      Corn.df$Repairs[j] <- ifelse(Corn.df$Year[j] == 1995, 
                                   Corn.df$Repairs[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - gradient,
                            ifelse(Corn.df$Year[j] == 1994,
                                   Corn.df$Repairs[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*2),
                            ifelse(Corn.df$Year[j] == 1993,
                                   Corn.df$Repairs[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*3),
                            ifelse(Corn.df$Year[j] == 1992,
                                   Corn.df$Repairs[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*4),
                            ifelse(Corn.df$Year[j] == 1991,
                                   Corn.df$Repairs[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*5),
                                   Corn.df$Repairs[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*6)) 
                            ))))
  
    }
    
  }
        
  
}

Corn.df.test <- Corn.df

for (i in 1:length(unique(Corn.df$Region))) {
  
  for(j in 1:nrow(Corn.df)) {
    
    if(Corn.df$Year[j] < 1996 & Corn.df$Region[j] == unique(Corn.df$Region)[i]){
      
      gradient <- (Corn.df$Fuel..lube..and.electricity[Corn.df$Year == 2000 & Corn.df$Region == unique(Corn.df$Region)[i]] - 
                     Corn.df$Fuel..lube..and.electricity[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]]) / 4
      
      Corn.df$Fuel..lube..and.electricity[j] <- ifelse(Corn.df$Year[j] == 1995, 
                                   Corn.df$Fuel..lube..and.electricity[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - gradient,
                                   ifelse(Corn.df$Year[j] == 1994,
                                          Corn.df$Fuel..lube..and.electricity[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*2),
                                          ifelse(Corn.df$Year[j] == 1993,
                                                 Corn.df$Fuel..lube..and.electricity[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*3),
                                                 ifelse(Corn.df$Year[j] == 1992,
                                                        Corn.df$Fuel..lube..and.electricity[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*4),
                                                        ifelse(Corn.df$Year[j] == 1991,
                                                               Corn.df$Fuel..lube..and.electricity[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*5),
                                                               Corn.df$Fuel..lube..and.electricity[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*6)) 
                                                 ))))
      
    }
    
  }
  
  
}


########################################################################

### what about some overheads? - filter out insurance and opportunity cost

########################################################################


setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Soy/Profit Loss Model')

Soy.files       <- list.files(pattern = '*.csv')
Soy.dat         <- lapply(Soy.files, read.csv, skip = 1)
Soy.dat         <- lapply(Soy.dat, function(x) {x[-c(grep('.*?([0-9]+).*', x$X)), ]})
names(Soy.dat)  <- substr(Soy.files, 1, nchar(Soy.files) - 4)

### Select key data

cnames          <- Soy.dat[[1]][, 1]
Soy.dat         <- lapply(Soy.dat, function(x) {data.frame(t(x))})
Soy.dat         <- lapply(Soy.dat, setNames, cnames)
Soy.dat         <- lapply(Soy.dat, function(x) {x[-c(1), ]})
Soy.dat         <- lapply(Soy.dat, function(x) {x[, c(18, 19)]})


### Corn

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Corn/Profit Loss Model')

Corn.files        <- list.files(pattern = '*.csv')
Corn.dat          <- lapply(Corn.files, read.csv, skip = 1)
Corn.dat          <- lapply(Corn.dat, function(x) {x[-c(grep('.*?([0-9]+).*', x$X)), ]})
names(Corn.dat)   <- substr(Corn.files, 1, nchar(Corn.files) - 4)

### Select key data

cnames           <- Corn.dat[[1]][, 1]
Corn.dat         <- lapply(Corn.dat, function(x) {data.frame(t(x))})
Corn.dat         <- lapply(Corn.dat, setNames, cnames)
Corn.dat         <- lapply(Corn.dat, function(x) {x[-c(1), ]})
Corn.dat         <- lapply(Corn.dat, function(x) {x[, c(19, 20)]})


### extrapolate back


Corn.baxtrapolate <- Corn.dat

while(nrow(Corn.baxtrapolate[[1]]) < 28){
  
  Corn.baxtrapolate <- lapply(Corn.baxtrapolate, function(x) {
    
    add_row(x)
  })
  
}

Corn.baxtrapolate <- lapply(Corn.baxtrapolate, function(x) {
  
  x[c(23:28, 22:1), ]
  
})


### Turn into one data frame

Corn.overheads         <- rbind.fill(Corn.baxtrapolate)

Corn.overheads         <- data.frame(apply(Corn.overheads, 2, function(x) {
  
  as.numeric(x)
  
}))

Corn.overheads$Year    <- 1990:2017
Corn.overheads$Region  <- rep(gsub('\\.csv', '', Corn.files), each = 28)


##############################################################################################

### Soy

##############################################################################################


Soy.baxtrapolate <- Soy.dat

while(nrow(Soy.baxtrapolate[[1]]) < 28){
  
  Soy.baxtrapolate <- lapply(Soy.baxtrapolate, function(x) {
    
    add_row(x)
  })
  
}

Soy.baxtrapolate <- lapply(Soy.baxtrapolate, function(x) {
  
  x[c(22:28, 21:1), ]
  
})


### Turn into one data frame

Soy.overheads         <- rbind.fill(Soy.baxtrapolate)
Soy.overheads         <- data.frame(apply(Soy.overheads, 2, function(x) {
  
  as.numeric(x)
  
}))

Soy.overheads$Year    <- rep(1990:2017, times= 7)
Soy.overheads$Region  <- rep(gsub('\\.csv', '', Soy.files), each = 28)


##########################################################################################################

### Subtract unwanted overheads from total

##########################################################################################################


Corn.df$Insurance   <- Corn.overheads$Taxes.and.insurance
Corn.df$Opportunity <- Corn.overheads$Opportunity.cost.of.land

Soy.df$Insurance    <- Soy.overheads$Taxes.and.insurance
Soy.df$Opportunity  <- Soy.overheads$Opportunity.cost.of.land


Corn.df.test <- Corn.df

for (i in 1:length(unique(Corn.df$Region))) {
  
  for(j in 1:nrow(Corn.df)) {
    
    if(Corn.df$Year[j] < 1996 & Corn.df$Region[j] == unique(Corn.df$Region)[i]){
      
      gradient <- (Corn.df$Insurance[Corn.df$Year == 2000 & Corn.df$Region == unique(Corn.df$Region)[i]] - 
                     Corn.df$Insurance[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]]) / 4
      
      Corn.df$Insurance[j] <- ifelse(Corn.df$Year[j] == 1995, 
                                                       Corn.df$Insurance[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - gradient,
                                                       ifelse(Corn.df$Year[j] == 1994,
                                                              Corn.df$Insurance[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*2),
                                                              ifelse(Corn.df$Year[j] == 1993,
                                                                     Corn.df$Insurance[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*3),
                                                                     ifelse(Corn.df$Year[j] == 1992,
                                                                            Corn.df$Insurance[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*4),
                                                                            ifelse(Corn.df$Year[j] == 1991,
                                                                                   Corn.df$Insurance[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*5),
                                                                                   Corn.df$Insurance[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*6)) 
                                                                     ))))
      
    }
    
  }
  
  
}

for (i in 1:length(unique(Corn.df$Region))) {
  
  for(j in 1:nrow(Corn.df)) {
    
    if(Corn.df$Year[j] < 1996 & Corn.df$Region[j] == unique(Corn.df$Region)[i]){
      
      gradient <- (Corn.df$Opportunity[Corn.df$Year == 2000 & Corn.df$Region == unique(Corn.df$Region)[i]] - 
                     Corn.df$Opportunity[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]]) / 4
      
      Corn.df$Opportunity[j] <- ifelse(Corn.df$Year[j] == 1995, 
                                     Corn.df$Opportunity[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - gradient,
                                     ifelse(Corn.df$Year[j] == 1994,
                                            Corn.df$Opportunity[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*2),
                                            ifelse(Corn.df$Year[j] == 1993,
                                                   Corn.df$Opportunity[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*3),
                                                   ifelse(Corn.df$Year[j] == 1992,
                                                          Corn.df$Opportunity[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*4),
                                                          ifelse(Corn.df$Year[j] == 1991,
                                                                 Corn.df$Opportunity[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*5),
                                                                 Corn.df$Opportunity[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*6)) 
                                                   ))))
      
    }
    
  }
  
  
}


for (i in 1:length(unique(Corn.df$Region))) {
  
  for(j in 1:nrow(Corn.df)) {
    
    if(Corn.df$Year[j] < 1996 & Corn.df$Region[j] == unique(Corn.df$Region)[i]){
      
      gradient <- (Corn.df$Total..allocated.overhead[Corn.df$Year == 2000 & Corn.df$Region == unique(Corn.df$Region)[i]] - 
                     Corn.df$Total..allocated.overhead[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]]) / 4
      
      Corn.df$Total..allocated.overhead[j] <- ifelse(Corn.df$Year[j] == 1995, 
                                       Corn.df$Total..allocated.overhead[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - gradient,
                                       ifelse(Corn.df$Year[j] == 1994,
                                              Corn.df$Total..allocated.overhead[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*2),
                                              ifelse(Corn.df$Year[j] == 1993,
                                                     Corn.df$Total..allocated.overhead[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*3),
                                                     ifelse(Corn.df$Year[j] == 1992,
                                                            Corn.df$Total..allocated.overhead[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*4),
                                                            ifelse(Corn.df$Year[j] == 1991,
                                                                   Corn.df$Total..allocated.overhead[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*5),
                                                                   Corn.df$Total..allocated.overhead[Corn.df$Year == 1996 & Corn.df$Region == unique(Corn.df$Region)[i]] - (gradient*6)) 
                                                     ))))
      
    }
    
  }
  
  
}


for (i in 1:length(unique(Soy.df$Region))) {
  
  for(j in 1:nrow(Soy.df)) {
    
    if(Soy.df$Year[j] < 1997 & Soy.df$Region[j] == unique(Soy.df$Region)[i]){
      
      gradient <- (Soy.df$Insurance[Soy.df$Year == 2000 & Soy.df$Region == unique(Soy.df$Region)[i]] - 
                     Soy.df$Insurance[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]]) / 4
      
      Soy.df$Insurance[j] <- ifelse(Soy.df$Year[j] == 1996, 
                                    Soy.df$Insurance[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]],
                                    ifelse(Soy.df$Year[j] == 1995, 
                                    Soy.df$Insurance[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]] - gradient,
                                     ifelse(Soy.df$Year[j] == 1994,
                                            Soy.df$Insurance[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]] - (gradient*2),
                                            ifelse(Soy.df$Year[j] == 1993,
                                                   Soy.df$Insurance[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]] - (gradient*3),
                                                   ifelse(Soy.df$Year[j] == 1992,
                                                          Soy.df$Insurance[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]] - (gradient*4),
                                                          ifelse(Soy.df$Year[j] == 1991,
                                                                 Soy.df$Insurance[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]] - (gradient*5),
                                                                 Soy.df$Insurance[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]] - (gradient*6)) 
                                                   )))))
      
    }
    
  }
  
  
}


for (i in 1:length(unique(Soy.df$Region))) {
  
  for(j in 1:nrow(Soy.df)) {
    
    if(Soy.df$Year[j] < 1997 & Soy.df$Region[j] == unique(Soy.df$Region)[i]){
      
      gradient <- (Soy.df$Opportunity[Soy.df$Year == 2000 & Soy.df$Region == unique(Soy.df$Region)[i]] - 
                     Soy.df$Opportunity[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]]) / 4
      
      Soy.df$Opportunity[j] <- ifelse(Soy.df$Year[j] == 1996, 
                                    Soy.df$Opportunity[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]],
                                    ifelse(Soy.df$Year[j] == 1995, 
                                           Soy.df$Opportunity[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]] - gradient,
                                           ifelse(Soy.df$Year[j] == 1994,
                                                  Soy.df$Opportunity[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]] - (gradient*2),
                                                  ifelse(Soy.df$Year[j] == 1993,
                                                         Soy.df$Opportunity[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]] - (gradient*3),
                                                         ifelse(Soy.df$Year[j] == 1992,
                                                                Soy.df$Opportunity[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]] - (gradient*4),
                                                                ifelse(Soy.df$Year[j] == 1991,
                                                                       Soy.df$Opportunity[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]] - (gradient*5),
                                                                       Soy.df$Opportunity[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]] - (gradient*6)) 
                                                         )))))
      
    }
    
  }
  
  
}


for (i in 1:length(unique(Soy.df$Region))) {
  
  for(j in 1:nrow(Soy.df)) {
    
    if(Soy.df$Year[j] < 1997 & Soy.df$Region[j] == unique(Soy.df$Region)[i]){
      
      gradient <- (Soy.df$Total..allocated.overhead[Soy.df$Year == 2000 & Soy.df$Region == unique(Soy.df$Region)[i]] - 
                     Soy.df$Total..allocated.overhead[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]]) / 4
      
      Soy.df$Total..allocated.overhead[j] <- ifelse(Soy.df$Year[j] == 1996, 
                                      Soy.df$Total..allocated.overhead[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]],
                                      ifelse(Soy.df$Year[j] == 1995, 
                                             Soy.df$Total..allocated.overhead[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]] - gradient,
                                             ifelse(Soy.df$Year[j] == 1994,
                                                    Soy.df$Total..allocated.overhead[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]] - (gradient*2),
                                                    ifelse(Soy.df$Year[j] == 1993,
                                                           Soy.df$Total..allocated.overhead[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]] - (gradient*3),
                                                           ifelse(Soy.df$Year[j] == 1992,
                                                                  Soy.df$Total..allocated.overhead[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]] - (gradient*4),
                                                                  ifelse(Soy.df$Year[j] == 1991,
                                                                         Soy.df$Total..allocated.overhead[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]] - (gradient*5),
                                                                         Soy.df$Total..allocated.overhead[Soy.df$Year == 1997 & Soy.df$Region == unique(Soy.df$Region)[i]] - (gradient*6)) 
                                                           )))))
      
    }
    
  }
  
  
}


###################################################

### Calculate revised overhead

###################################################

Soy.df$Overhead         <- Soy.df$Total..allocated.overhead - (Soy.df$Opportunity + Soy.df$Insurance)
Corn.df$Overhead        <- Corn.df$Total..allocated.overhead - (Corn.df$Opportunity + Corn.df$Insurance)

Soy.df$Operating.Costs  <- Soy.df[, 2] + Soy.df[, 3] + Soy.df[, 4] + Soy.df[, 5] + Soy.df[, 6] + Soy.df[, 7]
Soy.df$Total.Costs      <- Soy.df$Overhead + Soy.df$Operating.Costs
Soy.df$Margin           <- Soy.df$Primary.product.soybeans - Soy.df$Total.Costs

Corn.df$Operating.Costs  <- Corn.df[, 2] + Corn.df[, 3] + Corn.df[, 4] + Corn.df[, 5] + Corn.df[, 6] + Corn.df[, 7]
Corn.df$Total.Costs      <- Corn.df$Overhead + Corn.df$Operating.Costs
Corn.df$Margin           <- Corn.df$Total..gross.value.of.production - Corn.df$Total.Costs

Corn.df.test             <- Corn.df
Corn.df            <- Corn.df[, -c(grep('pred', colnames(Corn.df)))]

Soy.df.test              <- Soy.df
Soy.df             <- Soy.df[, -c(grep('.pred', colnames(Soy.df)))]


#setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Soy/Profit Loss Model')#
#
#write.csv(Soy.df, 'Soy_processed_profitlossv3.csv', row.names = FALSE)
#
#setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Corn/Profit Loss Model')
#
#write.csv(Corn.df, 'Corn_processed_profitlossv3.csv', row.names = FALSE)

###################################################################################################

### Next step: incorporate subsidy and see where there are thersholds between profit and land change

###################################################################################################







#######################################################################################################

### 8c) Govt subsidies

#######################################################################################################

### Insurance data

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Agricultural context')

Insurance.dat                <- read.csv('Insurance_processed.csv')
Insurance.dat                <- Insurance.dat[, -c(1)]

### Other payment data

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Agricultural context/Direct payments')


Payments.files               <- list.files(pattern = '*csv')
Payments.90s                 <- Payments.files[grep('90', Payments.files)]
Payments.00s                 <- Payments.files[grep('00', Payments.files)]
Payments.10s                 <- Payments.files[grep('10', Payments.files)]

##90s

Payments.90s.dat             <- lapply(Payments.90s, read.csv, skip = 4)
Payments.90s.dat             <- lapply(Payments.90s.dat, function(x) {x[1:14, ]})
Payments.90s.dat             <- lapply(Payments.90s.dat, function(x) {x[-c(1), ]})
Payments.states              <- lapply(Payments.90s.dat, function(x) {colnames(x)[1]})
cols                         <- as.character(Payments.90s.dat[[1]][, 1])

Payments.90s.dat             <- lapply(Payments.90s.dat, function(x) {
                                    apply(x[, -c(1, 5)], 2, function(y) {
                                      as.numeric(gsub(',', '', as.character(y)))
                                    })
                                  })
                                  
Payments.90s.dat             <- lapply(Payments.90s.dat, function(x) {data.frame(cols, x)})

##00s

Payments.00s.dat             <- lapply(Payments.00s, read.csv, skip = 4)
Payments.00s.dat             <- lapply(Payments.00s.dat, function(x) {x[1:22, ]})
Payments.00s.dat             <- lapply(Payments.00s.dat, function(x) {x[-c(1), ]})
Payments.states.00s          <- lapply(Payments.00s.dat, function(x) {colnames(x)[1]})
cols                         <- as.character(Payments.00s.dat[[1]][, 1])

Payments.00s.dat             <- lapply(Payments.00s.dat, function(x) {
  apply(x[, -c(1, 5)], 2, function(y) {
    as.numeric(gsub(',', '', as.character(y)))
  })
})

Payments.00s.dat             <- lapply(Payments.00s.dat, function(x) {data.frame(cols, x)})

#Payments.00s.dat             <- lapply(Payments.00s.dat, function(x) {x[-c(1), ]})

Payments.10s.dat             <- lapply(Payments.10s, read.csv, skip = 4)
Payments.10s.dat             <- lapply(Payments.10s.dat, function(x) {x[1:19, ]})
Payments.10s.dat             <- lapply(Payments.10s.dat, function(x) {x[-c(1), ]})
Payments.states.10s          <- lapply(Payments.10s.dat, function(x) {colnames(x)[1]})
cols                         <- as.character(Payments.10s.dat[[1]][, 1])

Payments.10s.dat             <- lapply(Payments.10s.dat, function(x) {
  apply(x[, -c(1, 5)], 2, function(y) {
    as.numeric(gsub(',', '', as.character(y)))
  })
})

Payments.10s.dat             <- lapply(Payments.10s.dat, function(x) {data.frame(cols, x)})
#Payments.10s.dat            <- lapply(Payments.10s.dat, function(x) {x[-c(1), ]})


################################################################################################

### Create Non-production specific subsidy by state by decade

################################################################################################

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Agricultural context')

Land.total        <- read.csv('All_Ag_Land.csv')
Land.total$Value  <- as.numeric(gsub(',', '', as.character(Land.total$Value)))
Land.total$Value  <- ifelse(is.na(Land.total$Value), 0, Land.total$Value)

Land.total$County.ANSI  <- formatC(Land.total$County.ANSI, width = 3, format = "d", flag = "0")
Land.total$County.ANSI  <- assign_other_county(Land.total)
Land.total$OverallANSI  <- paste0(Land.total$State.ANSI, Land.total$County.ANSI)
Land.total$USDAagregion <- map_agregion(Land.total)


  #### Make land stocks annual by state

Land.stocks.state            <- Land.total %>% group_by(.dots = c('State', 'Year')) %>% 
                                  summarise(Land = sum(Value, na.rm = TRUE))

Land.state.annual            <- data.frame(State = rep(unique(Land.stocks.state$State), times = length(c(1990:2017))),
                                           Year = rep(1990:2017, each  = length(unique(Land.stocks.state$State))), 
                                           Value = NA) 

Land.state.annual            <- merge.data.frame(Land.state.annual, Land.stocks.state, by.x = c('State', 'Year'),
                                                 by.y = c('State', 'Year'), all.x = TRUE)     

Land.state.annual            <- Land.state.annual[, -c(3)]


### linear extrapolation by state

Land.state.annual.wide            <- spread(Land.state.annual, key = State, value = Land)

Land.state.annual.wide[1:8, 2:51]     <- apply(Land.state.annual.wide[1:8, 2:51], 2, 
                                           function(x) {rep(x[8], times = 8)})

Land.state.annual.wide            <- data.frame(apply(Land.state.annual.wide, 2, 
                                           function(x) {na.locf(x)}))

### drop Hawaii and Alaska

Land.state.annual.wide            <- Land.state.annual.wide[, !colnames(Land.state.annual.wide) %in% c('HAWAII', 'ALASKA')]

######################################

  ### Create annualised data by County

######################################



Land.county.annual            <- data.frame(County = rep(unique(Land.stocks$OverallANSI), times = length(c(1990:2017))),
                                           Year = rep(1990:2017, each  = length(unique(Land.stocks$OverallANSI))), 
                                           Value = NA) 

Land.county.annual$State     <- ''

for (i in 1:nrow(Land.county.annual)) {

  try(
  
  Land.county.annual$State[i] <- as.character(Land.total[which(Land.county.annual$County[i] == Land.total$OverallANSI),]$State[1])
  )
  
  print(i)

}
  
Land.county.annual.test            <- merge.data.frame(Land.county.annual, Land.total, by.x = c('County', 'Year'),
                                                 by.y = c('OverallANSI', 'Year'), all.x = TRUE)     



Land.county.annual.test            <- Land.county.annual.test %>% dplyr::select(c(1, 2, 4, 23, 25))
  
### linear extrapolation by County

Land.county.annual.test            <- Land.county.annual.test %>% filter(!State.x %in% c('ALASKA', 'HAWAII'))
Land.county.wide                   <- spread(Land.county.annual.test[, c(1, 2, 4)], key = County, value = Value.y)

### this might be the cause

Land.county.wide[8, ]              <- ifelse(is.na(Land.county.wide[8, ]), 0, Land.county.wide[8, ])
Land.county.wide[13, ]             <- ifelse(is.na(Land.county.wide[13, ]), 0, Land.county.wide[13, ])
Land.county.wide[18, ]             <- ifelse(is.na(Land.county.wide[18, ]), 0, Land.county.wide[18, ])
Land.county.wide[23, ]             <- ifelse(is.na(Land.county.wide[23, ]), 0, Land.county.wide[23, ])
Land.county.wide[28, ]             <- ifelse(is.na(Land.county.wide[28, ]), 0, Land.county.wide[28, ])

Land.county.wide[1:8, 2:ncol(Land.county.wide)]     <- apply(Land.county.wide[1:8, 2:ncol(Land.county.wide)], 2, 
                                               function(x) {rep(x[8], times = 8)})



Land.county.wide            <- data.frame(apply(Land.county.wide, 2, 
                                                      function(x) {na.locf(x)}))

colnames(Land.county.wide)  <- gsub('X', '', colnames(Land.county.wide))


###########################################################################################################

  ### Calculate fixed subsidy by state

###########################################################################################################


Fixed.subsidy.90s            <- lapply(Payments.90s.dat, function(x) {
  apply(x[c(8, 9, 10, 12, 13), -c(1)], 2, function(y){
    sum(y, na.rm = TRUE)
  })
})    

Fixed.subsidy.90s                          <- data.frame(Fixed.subsidy.90s)
colnames(Fixed.subsidy.90s)                <- Payments.states

Fixed.subsidy.00s            <- lapply(Payments.00s.dat, function(x) {
  apply(x[c(8, 9, 11, 12, 13, 14, 20, 21), -c(1)], 2, function(y){
    sum(y, na.rm = TRUE)
  })
})    

Fixed.subsidy.00s                          <- data.frame(Fixed.subsidy.00s)
colnames(Fixed.subsidy.00s)                <- Payments.states.00s

Fixed.subsidy.10s            <- lapply(Payments.10s.dat, function(x) {
  apply(x[c(2, 5, 8, 9, 10, 16, 17, 18), -c(1)], 2, function(y){
    sum(y, na.rm = TRUE)
  })
})    

Fixed.subsidy.10s                          <- data.frame(Fixed.subsidy.10s)
colnames(Fixed.subsidy.10s)                <- Payments.states.10s

Fixed.subsidy.df                           <- rbind.fill(Fixed.subsidy.90s, Fixed.subsidy.00s, Fixed.subsidy.10s)
colnames(Fixed.subsidy.df)                 <- toupper(colnames(Fixed.subsidy.df))
Fixed.subsidy.df$Year                      <- 1990:2017
Fixed.subsidy.df                           <- Fixed.subsidy.df[, c(49, 1:48)]

Fixed.peracre                              <- (Fixed.subsidy.df / Land.state.annual.wide) * 1000
Fixed.peracre$Year                         <- 1990:2017



##############################################################################################################

### A1) Create per acre subsidy amt & allocate by county

##############################################################################################################

### calculate per acre

Fixed.peracre                              <- (Fixed.subsidy.df / Land.state.annual.wide) * 1000
Fixed.peracre$Year                         <- 1990:2017

### gather extrapolated county data

Land.annual.long            <- gather(Land.county.wide[, 2:ncol(Land.county.wide)])
Land.annual.long$Year       <- 1990:2017
Land.annual.long$State      <- Land.county.annual.test$State.x

teg <- apply(Land.annual.long, 1, function(x) {
  as.numeric(as.character(x[2])) * as.numeric(as.character(Fixed.peracre[which(Fixed.peracre$Year == x[3]), which(colnames(Fixed.peracre) == x[4])]))
})

Land.annual.long$subsidy    <- teg
Land.annual.long$subsidy    <- as.numeric(Land.annual.long$subsidy)


########################################################################################################################

### *** REDO WITH ANNUAL INCREMENTALISED CHANGES IN LAND FOOTPRINT ***

########################################################################################################################


setwd('E:/Modules/Dissertation/Data/Processed Dat')
Land.annual.long <- read.csv('Generic_subsidy.csv')

teg <- spread(Land.annual.long[, c(1, 2, 3)], key = key, value = value)

targetrows <- c(8, 13, 18, 23, 28)
counter    <- 0


  for(i in 1:(length(targetrows)-1)) {
    
    t <- targetrows[i+1] - targetrows[i]
    z <- rep(1, times = t)
    
    for(j in 2:ncol(teg)) {
    
      if(!is.na(teg[targetrows[i+1], j] - teg[targetrows[i], j] / 5)) {
      
      f <- (teg[targetrows[i+1], j] - teg[targetrows[i], j]) / 5
      
      z[1] <- teg[targetrows[i], j]
      z[2] <- teg[targetrows[i]+1, j] + f
      z[3] <- teg[targetrows[i]+2, j] + f*2
      z[4] <- teg[targetrows[i]+3, j] + f*3
      z[5] <- teg[targetrows[i]+4, j] + f*4
      z[6] <- teg[targetrows[i+1], j]
      
      teg[c(targetrows[i]:targetrows[i+1]), j] <- as.numeric(z)
      
      } else {
        
        counter <- counter + 1
      }
      
  }
  
  print(i, counter)
  
}





### gather extrapolated county data

teg            <- gather(teg[, 2:ncol(teg)])
teg$Year       <- 1990:2017

teg$State      <- ''

for(i in 1:nrow(teg)) {
  
  teg$State[i] <- unique(as.character(Land.stocks[which(Land.stocks$OverallANSI == teg$key[i]), 5]))
  
  print(i)
  
}

#teg2 <- teg
#teg  <- teg2

peracre2 <- apply(teg, 1, function(x) {
  as.numeric(x[2]) * Fixed.peracre[which(Fixed.peracre$Year == x[3]), which(gsub('\\.', ' ',colnames(Fixed.peracre)) == x[4])]
})

teg$subsidy     <- peracre2

statez <- teg %>% group_by(Year) %>% summarise(subsidiez = sum(subsidy, na.rm =TRUE))

plot(1990:2017, statez$subsidiez)



##############################################################################################################

### A1) Allocate subsidy by commodity

##############################################################################################################

############ read in data


setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Agricultural context')

ARCO               <- read.csv('ARCCO_Data_2019_0308.csv', skip = 3)
colnames(ARCO)     <- substr(colnames(ARCO), 1, nchar(colnames(ARCO)))


setwd('E:/Modules/Dissertation/Data/Processed Dat')

direct.subsidy     <- read.csv('incrementalised_directsubsidy.csv')


setwd('E:/Modules/Dissertation/Data/Model Ready')

Corn               <- read.csv('Corn_climate_CRP.csv')
Corn               <- Corn[, -c(1, 2)]

Soy                <- read.csv('Soy_CRP_climate.csv')
Soy                <- Soy[, -c(1)]


########################################################################################

### ARCO

########################################################################################

############ Calculate commodity-specific subsidy by county


### extract payment rate

Payment.rate           <- ARCO %>% select(grep('Payment.Rate', colnames(ARCO)))
Payment.rate           <- apply(Payment.rate, 2, function(x) {as.numeric(gsub('\\$', '', as.character(x)))})
Payment.rate           <- data.frame(Payment.rate)
Payment.rate$FIPS      <- ARCO$ST_Cty
Payment.rate$State     <- ARCO$State.Name
Payment.rate$Commodity <- ARCO$Crop.Name

Payment.actual     <- Payment.rate %>% select(-c(grep('Maximum', colnames(Payment.rate))))
Payment.actual     <- Payment.actual %>% select(-c(grep('Formula', colnames(Payment.actual))))


### multiply by county acreage of each commodity

Payment.Corn       <- Payment.actual %>% filter(Commodity == 'Corn')
Payment.Soy        <- Payment.actual %>% filter(Commodity == 'Soybeans')
Years              <- c(2014:2017)


Corn$ACRO_CO.Subsidy <- 0

for(i in 1:length(Years)) {

  for(j in 1:nrow(Corn)) {
  
    if(Corn$Year[j] == Years[i]) {
    
    try(
    
    Corn$ACRO_CO.Subsidy[j]    <- Corn$Acres_Planted[j] * Payment.Corn[which(Payment.Corn$FIPS == Corn$OverallANSI[j]), i], 
    
    silent = TRUE

  )

    }
      
  }
  
  print(i)

}


Soy$ACRO_CO.Subsidy <- 0

for(i in 1:length(Years)) {
  
  for(j in 1:nrow(Soy)) {
    
    if(Soy$Year[j] == Years[i]) {
      
      try(
        
        Soy$ACRO_CO.Subsidy[j]    <- Soy$Acres_Planted[j] * Payment.Soy[which(Payment.Soy$FIPS == Soy$OverallANSI[j]), i], 
        
        silent = TRUE
        
      )
      
    }
    
  }
  
  print(i)
  
}


########################################################################################

### Pre-1997 commodity payments

########################################################################################

### create Corn / Sorghum / Barley ratio for feed grains

setwd('E:/Modules/Dissertation/Data/Processed Dat')

Commods                    <- read.csv('extra_commods_barley.csv')
Commods                    <- Commods[, -c(1)]
Commods.wide               <- spread(Commods, key = Commodity, value = Acres.Planted)

Corn.state                 <- Corn %>% dplyr::group_by(.dots = c('Year', 'State')) %>%
                                  dplyr::summarise(Acres_Planted = sum(Acres_Planted, na.rm = TRUE))

Commods$CORN               <- 0

for(i in 1:nrow(Commods)) {
  
  try(
  
  Commods$Corn[i] <- Corn.state[which(as.character(Corn.state$Year) == as.character(Commods$Year)[i] & 
                                        as.character(Corn.state$State) == as.character(Commods$State)[i]), 3],
  
  silent = TRUE
  
  )
  
  print(i)
  
}

Commods.wide               <- spread(Commods, key = Commodity, value = Acres.Planted)
Commods.wide$CORN          <- sapply(Commods.wide$Corn, as.numeric)
Commods.wide$CORN          <- as.numeric(Commods.wide$CORN)
Commods.wide               <- Commods.wide[, -c(4)]
Commods.wide               <- apply(Commods.wide, 2, function(x) {ifelse(is.na(x), 0, x)})
Commods.wide               <- data.frame(Commods.wide)
Commods.wide[, 3:9]        <- data.frame(apply(Commods.wide[, 3:9], 2, function(x) {as.numeric(as.character(x))}))

Commods.wide$Corn.feedgrain.Ratio <- Commods.wide$CORN / (Commods.wide$SORGHUM + 
                                        Commods.wide$CORN + Commods.wide$BARLEY)


### load payment data


setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Agricultural context/Direct payments')


Payments.files               <- list.files(pattern = '*.csv')
Payments.90s                 <- Payments.files[grep('90', Payments.files)]
Payments.00s                 <- Payments.files[grep('00', Payments.files)]


##90s

Payments.90s.dat             <- lapply(Payments.90s, read.csv, skip = 4)
Payments.90s.dat             <- lapply(Payments.90s.dat, function(x) {x[1:14, ]})
Payments.90s.dat             <- lapply(Payments.90s.dat, function(x) {x[-c(1), ]})
Payments.states              <- lapply(Payments.90s.dat, function(x) {colnames(x)[1]})
cols                         <- as.character(Payments.90s.dat[[1]][, 1])

Payments.90s.dat             <- lapply(Payments.90s.dat, function(x) {
  apply(x[, -c(1, 5)], 2, function(y) {
    as.numeric(gsub(',', '', as.character(y)))
  })
})

Payments.90s.dat             <- lapply(Payments.90s.dat, function(x) {data.frame(cols, x)})

##00s

Payments.00s.dat             <- lapply(Payments.00s, read.csv, skip = 4)
Payments.00s.dat             <- lapply(Payments.00s.dat, function(x) {x[1:22, ]})
Payments.00s.dat             <- lapply(Payments.00s.dat, function(x) {x[-c(1), ]})
Payments.states.00s          <- lapply(Payments.00s.dat, function(x) {colnames(x)[1]})
cols                         <- as.character(Payments.00s.dat[[1]][, 1])

Payments.00s.dat             <- lapply(Payments.00s.dat, function(x) {
  apply(x[, -c(1, 5)], 2, function(y) {
    as.numeric(gsub(',', '', as.character(y)))
  })
})

Payments.00s.dat             <- lapply(Payments.00s.dat, function(x) {data.frame(cols, x)})


###################################################################################################

### Merge payment data with acreage by state

###################################################################################################

Payments.90s.dat            <- lapply(Payments.90s.dat, function(x) {x[2:7, ]})

Payments.90s.df             <- rbind.fill(Payments.90s.dat)

Payments.90s.df$State       <- rep(as.character(Payments.states), each =6)
Payments.90s.df$State       <- toupper(Payments.90s.df$State)

Commods.wide$Corn.Payment   <- 0

for(i in 1:nrow(Commods.wide)) {

  try(
  
Commods.wide$Corn.Payment[i]<- Payments.90s.df[which(Payments.90s.df$State == Commods.wide$State[i] & 
                                                        Payments.90s.df$cols == 'Feed grains'),
                                                substr(colnames(Payments.90s.df), 2, nchar(colnames(Payments.90s.df))) == as.character(Commods.wide$Year)[i]],
silent = TRUE

  )

  print(i)
  
}

Commods.wide$Corn.Payment <- Commods.wide$Corn.Payment * Commods.wide$Corn.Sorghum.Ratio
Commods.wide$Corn.Payment <- Commods.wide$Corn.Payment * 1000
Commods.wide$Corn.Peracre <- Commods.wide$Corn.Payment / Commods.wide$CORN

#############################################################################################################

#### Convert statewide payments to per acre payments by county

#############################################################################################################

Corn$Old_Commodity_Payment <- 0

for(i in 1:nrow(Corn)) {
  
  if(Corn$Year < 2000) {
    
    try(
      
      Corn$Old_Commodity_Payment[i] <- Corn$Acres_Planted[i] * Commods.wide[which(
        Commods.wide$Year == Corn$Year[i] & as.character(Commods.wide$State) == as.character(Corn$State)[i]
      ), 12]
      
      , silent = TRUE
      
    )
  }
  
  print(i)
  
}

#############################################################################################################

#### Merge insurance data

#############################################################################################################


Corn$Insurance <- 0
Soy$Insurance  <- 0

for (i in 1:nrow(Corn)) {
  
  try(
  
  Corn$Insurance[i] <- Insurance.dat[which(Insurance.dat$Commodity.Name == 'CORN' &
                                             Insurance.dat$FIPS == Corn$OverallANSI[i] &
                                             Insurance.dat$ï..Commodity.Year == Corn$Year[i]),24 ],
  
  silent = TRUE
  
  )
  
  print(i)
  
}

for (i in 1:nrow(Soy)) {
  
  try(
    
    Soy$Insurance[i] <- Insurance.dat[which(Insurance.dat$Commodity.Name == 'SOYBEANS' &
                                               Insurance.dat$FIPS == Soy$OverallANSI[i] &
                                               Insurance.dat$ï..Commodity.Year == Soy$Year[i]),24 ],
    
    silent = TRUE
    
  )
  
  print(i)
  
}



#########################################################################################################

### Add all other data to sheets

#########################################################################################################

Corn$State <- as.character(Corn$State)

Corn$Direct.Subsidy  <- 0
Soy$Direct.Subsidy   <- 0

teg$key <- as.numeric(teg$key)

for (i in 1:nrow(Corn)) {
  
  try(
    
    Corn$Direct.Subsidy[i] <- teg[which(teg$FIPS == Corn$OverallANSI[i] &
                                                     direct.subsidy$Year == Corn$Year[i]),5 ] *
                            (
                              Corn$Acres_Planted[i] / 
                              direct.subsidy[which(direct.subsidy$FIPS == Corn$OverallANSI[i] &
                                                     direct.subsidy$Year == Corn$Year[i]),2 ]),
    
    silent = TRUE
    
  )
  
  print(i)
  
}


Corn.merge <- merge(Corn, teg, by.x = c('OverallANSI', 'Year'), by.y = c('key', 'Year'))
Corn$Direct.Subsidy <- Corn.merge$subsidy

teg$Corn <- 0

for(i in 1:nrow(teg)) {

  try(
  
teg$Corn[i] <-   Corn$Acres_Planted[which(Corn$Year == teg$Year[i] & 
                                            Corn$OverallANSI == teg$key[i])] / teg$subsidy[i]
  
)

}


######################################################################


for (i in 1:nrow(Soy)) {

    try(
      
      Soy$Direct.Subsidy[i] <- direct.subsidy[which(direct.subsidy$FIPS == Soy$OverallANSI[i] &
                                                       direct.subsidy$Year == Soy$Year[i]),5 ] *
        (
          Soy$Acres_Planted[i] / direct.subsidy[which(direct.subsidy$FIPS == Soy$OverallANSI[i] &
                                                            direct.subsidy$Year == Soy$Year[i]),2 ]),
    
    silent = TRUE
    
  )
  
  print(i)
  
}

Corn$Total_Subsidy <- Corn$ACRO_CO.Subsidy + Corn$Old_Commodity_Payment + Corn$Insurance + Corn$Direct.Subsidy
Soy$Total_Subsidy  <- Soy$ACRO_CO.Subsidy + Soy$Insurance + Soy$Direct.Subsidy

Corn.year <- Corn %>% dplyr::group_by(Year) %>% dplyr::summarise(Total_Subsidy = sum(Total_Subsidy, na.rm = TRUE))
Soy.year  <- Soy %>% dplyr::group_by(Year) %>% dplyr::summarise(Total_Subsidy = median(Total_Subsidy, na.rm = TRUE))



##############################################################################################################

### redo direct subsidies

##############################################################################################################

setwd('E:/Modules/Dissertation/Data/Model Ready')

Corn               <- read.csv('Corn_subsidies.csv')

Soy                <- read.csv('Soy_subsidies.csv')

#setwd('E:/Modules/Dissertation/Data/Processed Dat')
#
#direct.subsidy     <- read.csv('incrementalised_directsubsidy.csv')
#
#
#Corn$Direct.Subsidy              <- 0
#
#Soy$Direct.Subsidy               <- 0
#
#Corn <- Corn[!is.na(Corn$County), ]
#
####
#
#Direct.Corn <- list()
#
#for(i in 1:nrow(Corn)) {
#
#Direct.Corn[[i]] <- direct.subsidy[which(direct.subsidy$FIPS == Corn$OverallANSI[i] & 
#                                          direct.subsidy$Year == Corn$Year[i]), 5]
#  
#}
#
#
#Direct.Soy <- list()
#
#for(i in 1:nrow(Soy)) {
#  
#  Direct.Soy[[i]] <- direct.subsidy[which(direct.subsidy$FIPS == Soy$OverallANSI[i] & 
#                                            direct.subsidy$Year == Soy$Year[i]), 5]
#  
#  
#  print(i)
#}
#
#Soy$Direct.Subsidy  <- as.numeric(Direct.Soy)
#Corn$Direct.Subsidy <- as.numeric(Direct.Corn)
#
###?WTF
#
#WTF <- list()
#
#for(i in 1:nrow(Corn)){
#
#  WTF[[i]] <- Corn$Direct.Subsidy[i] == direct.subsidy$Subsidy[which(direct.subsidy$Year == 
#                                              Corn$Year[i] & direct.subsidy$FIPS == Corn$OverallANSI[i])]
#  
#}
#

####################################################################################################

### Impute Irrigation data for profit model

####################################################################################################

test<- list()

for (i in 1:length(unique(Corn.profit$Region))) {
  
  for(j in 1:nrow(Corn.profit)) {
  
Corn.profit$Irrigated..percent.of.acres.[j] <- ifelse(Corn.profit$Year[j] < 1997 & Corn.profit$Region[j] == unique(Corn.profit$Region)[i],
                                                    Corn.profit$Irrigated..percent.of.acres.[Corn.profit$Year == 1997 & Corn.profit$Region == unique(Corn.profit$Region)[i]], 
                                                                                             Corn.profit$Irrigated..percent.of.acres.[j])

  }

}


Corn.profit$Dryland..percent.of.acres. <- 100 - Corn.profit$Irrigated..percent.of.acres.

for (i in 1:length(unique(Corn.profit$Region))) {
  
  for(j in 1:nrow(Corn.profit)) {
    
    Corn.profit$Enterprise.size..planted.acres.[j] <- ifelse(Corn.profit$Year[j] < 1997 & Corn.profit$Region[j] == unique(Corn.profit$Region)[i],
                                                          Corn.profit$Enterprise.size..planted.acres.[Corn.profit$Year == 1997 & Corn.profit$Region == unique(Corn.profit$Region)[i]], 
                                                          Corn.profit$Enterprise.size..planted.acres.[j])
    
  }
  
}

for (i in 1:length(unique(Soy.profit$Region))) {
  
  for(j in 1:nrow(Soy.profit)) {
    
    Soy.profit$Irrigated..percent.of.acres.[j] <- ifelse(Soy.profit$Year[j] < 1997 & Soy.profit$Region[j] == unique(Soy.profit$Region)[i],
                                                         Soy.profit$Irrigated..percent.of.acres.[Soy.profit$Year == 1997 & Soy.profit$Region == unique(Soy.profit$Region)[i]], 
                                                         Soy.profit$Irrigated..percent.of.acres.[j])
    
  }
  
}


Soy.profit$Dryland..percent.of.acres. <- 100 - Soy.profit$Irrigated..percent.of.acres.

for (i in 1:length(unique(Soy.profit$Region))) {
  
  for(j in 1:nrow(Soy.profit)) {
    
    Soy.profit$Enterprise.size..planted.acres.[j] <- ifelse(Soy.profit$Year[j] < 1997 & Soy.profit$Region[j] == unique(Soy.profit$Region)[i],
                                                         Soy.profit$Enterprise.size..planted.acres.[Soy.profit$Year == 1997 & Soy.profit$Region == unique(Soy.profit$Region)[i]], 
                                                         Soy.profit$Enterprise.size..planted.acres.[j])
    
  }
  
}

##################################################################################################################

####################################################################################################

### 9) Integrate subsidies and profit loss system

####################################################################################################

#################################################################################################################

setwd('E:/Modules/Dissertation/Data/Model Ready')

Soy           <- read.csv('Soy_subsidies.csv')
Corn          <- read.csv('Corn_subsidies.csv')

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Soy/Profit Loss Processed')

Soy.profit    <- read.csv('Soy_processed_profitlossv3.csv')
Soy.profit    <- Soy.profit[, -c(8:12)] ## remove old overhead, total costs & margins

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Corn/Profit Loss Processed')

Corn.profit   <- read.csv('Corn_processed_profitlossv3.csv')
Corn.profit   <- Corn.profit[, -c(8:12)] ## remove old overhead, total costs & margins

setwd('E:/Modules/Dissertation/Data/Processed Dat')

Fixed.peracre <- read.csv('Fixed_peracre_subsidy.csv')


#################################################################################################

### 9a) Create county-level profit loss models

#################################################################################################

Corn.county.profitloss  <- Corn[, c(1, 3, 4, 10, 11, 12, 13, 14, 16, 17, 18, 19,
                                   39, 40, 41)]

Corn.county.profitloss  <- data.frame(Corn.county.profitloss %>% filter(USDAagregion %in% c(1:6)))
Corn.profit$Region      <- as.character(Corn.profit$Region)

Soy.county.profitloss  <- Soy[, c(1, 2, 4, 10, 11, 12, 13, 14, 16, 17, 18, 19, 40, 41)]
Soy.county.profitloss  <- data.frame(Soy.county.profitloss %>% filter(USDAagregion %in% c(1:6, 9)))
Soy.profit$Region      <- as.character(Soy.profit$Region)

#################################################################################################

### Feed in Agregion-level data to counties

#################################################################################################

### add cols

Soy.county.profitloss$Seed       <- 0
Soy.county.profitloss$Fertiliser <- 0
Soy.county.profitloss$Chemicals  <- 0
Soy.county.profitloss$Customservices <- 0
Soy.county.profitloss$Repairs    <- 0
Soy.county.profitloss$Overhead   <- 0

Corn.county.profitloss$Seed       <- 0
Corn.county.profitloss$Fertiliser <- 0
Corn.county.profitloss$Chemicals  <- 0
Corn.county.profitloss$Customservices <- 0
Corn.county.profitloss$Repairs    <- 0
Corn.county.profitloss$Overhead   <- 0


Corn.county.profitloss <- data.frame(Corn.county.profitloss %>% 
                                    mutate(Agregion.name = ifelse(USDAagregion == 1, 'Heartland',
                                                                  ifelse(USDAagregion == 2, 'Northern Crescent',
                                                                         ifelse(USDAagregion == 3, 'Northern Great Plains',
                                                                                ifelse(USDAagregion == 4, 'Prarie Gateway', 
                                                                                       ifelse(USDAagregion == 5, 'Eastern Uplands', 
                                                                                              ifelse(USDAagregion == 6, 'Southern Seabord',
                                                                                                     ifelse(USDAagregion == 7, 'Fruitful Rim', 
                                                                                                            ifelse(USDAagregion == 8, 'Basin and Range', 
                                                                                                                   ifelse(USDAagregion == 9, 'Mississipi Portal', 
                                                                                                                          ifelse(is.na(USDAagregion), NA, ''))))))))))))


Soy.county.profitloss <- data.frame(Soy.county.profitloss %>% 
                                       mutate(Agregion.name = ifelse(USDAagregion == 1, 'Heartland',
                                                                     ifelse(USDAagregion == 2, 'Northern Crescent',
                                                                            ifelse(USDAagregion == 3, 'Northern Great Plains',
                                                                                   ifelse(USDAagregion == 4, 'Prarie Gateway', 
                                                                                          ifelse(USDAagregion == 5, 'Eastern Uplands', 
                                                                                                 ifelse(USDAagregion == 6, 'Southern Seaboard',
                                                                                                        ifelse(USDAagregion == 7, 'Fruitful Rim', 
                                                                                                               ifelse(USDAagregion == 8, 'Basin and Range', 
                                                                                                                      ifelse(USDAagregion == 9, 'Mississipi Portal', 
                                                                                                            ifelse(is.na(USDAagregion), NA, ''))))))))))))


Soy.profit$Region <- as.character(Soy.profit$Region)


for(j in 1:nrow(Soy.county.profitloss)) {

Soy.county.profitloss$Seed[j] <- Soy.profit$Seed[Soy.profit$Year == Soy.county.profitloss$Year[j] &
                                            as.character(Soy.profit$Region) == Soy.county.profitloss$Agregion.name[j]] 

Soy.county.profitloss$Fertiliser[j] <- Soy.profit$Fertilizer.áµf.[Soy.profit$Year == Soy.county.profitloss$Year[j] &
                                                       Soy.profit$Region == Soy.county.profitloss$Agregion.name[j]]

Soy.county.profitloss$Customservices[j] <- Soy.profit$Custom.services[Soy.profit$Year == Soy.county.profitloss$Year[j] &
                                                                    Soy.profit$Region == Soy.county.profitloss$Agregion.name[j]]

Soy.county.profitloss$Fuel[j] <- Soy.profit$Fuel..lube..and.electricity[Soy.profit$Year == Soy.county.profitloss$Year[j] &
                                                                        Soy.profit$Region == Soy.county.profitloss$Agregion.name[j]]

Soy.county.profitloss$Chemicals[j] <- Soy.profit$Chemicals[Soy.profit$Year == Soy.county.profitloss$Year[j] &
                                                                          Soy.profit$Region == Soy.county.profitloss$Agregion.name[j]]

Soy.county.profitloss$Repairs[j] <- Soy.profit$Repairs[Soy.profit$Year == Soy.county.profitloss$Year[j] &
                                                             Soy.profit$Region == Soy.county.profitloss$Agregion.name[j]]

Soy.county.profitloss$Overhead[j] <- Soy.profit$Overhead[Soy.profit$Year == Soy.county.profitloss$Year[j] &
                                                           Soy.profit$Region == Soy.county.profitloss$Agregion.name[j]]


print(j)

}


for(j in 1:nrow(Corn.county.profitloss)) {
  
  Corn.county.profitloss$Seed[j] <- Corn.profit$Seed[Corn.profit$Year == Corn.county.profitloss$Year[j] &
                                                     as.character(Corn.profit$Region) == Corn.county.profitloss$Agregion.name[j]] 
  
  Corn.county.profitloss$Fertiliser[j] <- Corn.profit$Fertilizer.áµf.[Corn.profit$Year == Corn.county.profitloss$Year[j] &
                                                                       Corn.profit$Region == Corn.county.profitloss$Agregion.name[j]]
  
  Corn.county.profitloss$Customservices[j] <- Corn.profit$Custom.services[Corn.profit$Year == Corn.county.profitloss$Year[j] &
                                                                            Corn.profit$Region == Corn.county.profitloss$Agregion.name[j]]
  
  Corn.county.profitloss$Fuel[j] <- Corn.profit$Fuel..lube..and.electricity[Corn.profit$Year == Corn.county.profitloss$Year[j] &
                                                                              Corn.profit$Region == Corn.county.profitloss$Agregion.name[j]]
  
  Corn.county.profitloss$Chemicals[j] <- Corn.profit$Chemicals[Corn.profit$Year == Corn.county.profitloss$Year[j] &
                                                                 Corn.profit$Region == Corn.county.profitloss$Agregion.name[j]]
  
  Corn.county.profitloss$Repairs[j] <- Corn.profit$Repairs[Corn.profit$Year == Corn.county.profitloss$Year[j] &
                                                             Corn.profit$Region == Corn.county.profitloss$Agregion.name[j]]
  
  
  Corn.county.profitloss$Overhead[j] <- Corn.profit$Overhead[Corn.profit$Year == Corn.county.profitloss$Year[j] &
                                                              Corn.profit$Region == Corn.county.profitloss$Agregion.name[j]]
  
  print(j)
  
}

####### Allocate non-commodity specific subsidy by state


Corn.county.profitloss$Generic_subsidy <- 0
Soy.county.profitloss$Generic_subsidy  <- 0
  
for(j in 1:nrow(Corn.county.profitloss)) {
  
  Corn.county.profitloss$Generic_subsidy[j] <- Fixed.peracre[Fixed.peracre$Year == Corn.county.profitloss$Year[j], 
                                                    gsub('\\.', ' ', colnames(Fixed.peracre)) == Corn.county.profitloss$State[j]]
  
  print(j)
  
}

for(j in 1:nrow(Soy.county.profitloss)) {
  
  Soy.county.profitloss$Generic_subsidy[j] <- Fixed.peracre[Fixed.peracre$Year == Soy.county.profitloss$Year[j], gsub('\\.', ' ', 
                                                                                                                      colnames(Fixed.peracre)) == Soy.county.profitloss$State[j]]
  
  print(j)
  
}

#########################################################################################################

### Calculate per acre margin with / without subsidy

#########################################################################################################


Corn.county.profitloss$Grossvalue            <- Corn.county.profitloss$Yield * Corn.county.profitloss$CORN..GRAIN...PRICE.RECEIVED..MEASURED.IN.....BU
Corn.county.profitloss$Margin                <- Corn.county.profitloss$Grossvalue - (Corn.county.profitloss$Overhead + Corn.county.profitloss$Seed + Corn.county.profitloss$Fertiliser + Corn.county.profitloss$Chemicals + Corn.county.profitloss$Customservices + Corn.county.profitloss$Repairs + Corn.county.profitloss$Fuel)
Corn.county.profitloss$Insurance             <- Corn.county.profitloss$Insurance / Corn.county.profitloss$Acres_Planted
Corn.county.profitloss$ACRO_CO.Subsidy       <- Corn.county.profitloss$ACRO_CO.Subsidy / Corn.county.profitloss$Acres_Planted
Corn.county.profitloss$Old_Commodity_Payment <- Corn.county.profitloss$Old_Commodity_Payment/ Corn.county.profitloss$Acres_Planted
Corn.county.profitloss$Margin_withsubsidy    <- Corn.county.profitloss$Margin + Corn.county.profitloss$ACRO_CO.Subsidy + Corn.county.profitloss$Insurance + Corn.county.profitloss$Old_Commodity_Payment + Corn.county.profitloss$Generic_subsidy

### loss ratio

Corn.county.profitloss$Grossvalue_weighted       <- Corn.county.profitloss$Grossvalue * (Corn.county.profitloss$CORN..GRAIN...ACRES.HARVESTED / Corn.county.profitloss$Acres_Planted) 
Corn.county.profitloss$Margin_subsidyweighted    <- Corn.county.profitloss$Grossvalue_weighted - (Corn.county.profitloss$Overhead + Corn.county.profitloss$Seed + Corn.county.profitloss$Fertiliser + Corn.county.profitloss$Chemicals + Corn.county.profitloss$Customservices + Corn.county.profitloss$Repairs + Corn.county.profitloss$Fuel) + 
                                                      Corn.county.profitloss$ACRO_CO.Subsidy + Corn.county.profitloss$Insurance + Corn.county.profitloss$Old_Commodity_Payment + Corn.county.profitloss$Generic_subsidy

Soy.county.profitloss$Grossvalue            <- Soy.county.profitloss$Yield * Soy.county.profitloss$SOYBEANS...PRICE.RECEIVED..MEASURED.IN.....BU
Soy.county.profitloss$Margin                <- Soy.county.profitloss$Grossvalue - (Soy.county.profitloss$Overhead + Soy.county.profitloss$Seed + Soy.county.profitloss$Fertiliser + Soy.county.profitloss$Chemicals + Soy.county.profitloss$Customservices + Soy.county.profitloss$Repairs + Soy.county.profitloss$Fuel)
Soy.county.profitloss$Insurance             <- Soy.county.profitloss$Insurance / Soy.county.profitloss$Acres_Planted
Soy.county.profitloss$ACRO_CO.Subsidy       <- Soy.county.profitloss$ACRO_CO.Subsidy / Soy.county.profitloss$Acres_Planted
Soy.county.profitloss$Margin_withsubsidy    <- Soy.county.profitloss$Margin + Soy.county.profitloss$ACRO_CO.Subsidy + Soy.county.profitloss$Insurance + Soy.county.profitloss$Generic_subsidy

### loss ratio

Soy.county.profitloss$Grossvalue_weighted       <- Soy.county.profitloss$Grossvalue * (Soy.county.profitloss$SOYBEANS...ACRES.HARVESTED / Soy.county.profitloss$Acres_Planted) 
Soy.county.profitloss$Margin_subsidyweighted    <- Soy.county.profitloss$Grossvalue_weighted - (Soy.county.profitloss$Overhead + Soy.county.profitloss$Seed + Soy.county.profitloss$Fertiliser + Soy.county.profitloss$Chemicals + Soy.county.profitloss$Customservices + Soy.county.profitloss$Repairs + Soy.county.profitloss$Fuel) + 
  Soy.county.profitloss$ACRO_CO.Subsidy + Soy.county.profitloss$Insurance + Soy.county.profitloss$Generic_subsidy


### Irrigated pct

Soy.county.profitloss$Irrigated_pct         <- Soy.county.profitloss$SOYBEANS..IRRIGATED...ACRES.PLANTED / Soy.county.profitloss$Acres_Planted
Soy.county.profitloss$Irrigated_pct         <- ifelse(is.na(Soy.county.profitloss$Irrigated_pct), 0, Soy.county.profitloss$Irrigated_pct)

Corn.county.profitloss$Irrigated_pct        <- Corn.county.profitloss$CORN..IRRIGATED...ACRES.PLANTED / Corn.county.profitloss$Acres_Planted
Corn.county.profitloss$Irrigated_pct        <- ifelse(is.na(Corn.county.profitloss$Irrigated_pct), 0, Corn.county.profitloss$Irrigated_pct)


setwd('E:/Modules/Dissertation/Data/Model Ready/Economic model')
#write.csv(Corn.county.profitloss, 'Corn_economic.csv', row.names = FALSE)
#write.csv(Soy.county.profitloss, 'Soy_economic.csv', row.names = FALSE)

Soy$Insurance       <- Soy$Insurance / Soy$Acres_Planted
Soy$ACRO_CO.Subsidy <- Soy$ACRO_CO.Subsidy / Soy$Acres_Planted

#
test <- merge(Soy.economic, Soy[, c(1,2, 40)], by = c('OverallANSI', 'Year'), all.x = TRUE)
test$ACRO_CO.Subsidy <- test$ACRO_CO.Subsidy.y
test           <- test[, -c(13, 30)]
#
#write.csv(test, 'Soy_economic.csv', row.names = FALSE)


#**********************************************************************************************************

############################################################################################################

### 10) Create data sheet for turning expectation into land use (extensive)

############################################################################################################

#***********************************************************************************************************

fill_na           <- colwise(function(x) {ifelse(is.na(x), 0, x)})


setwd('E:/Modules/Dissertation/Data/Model Ready/Economic model')

Corn.economic     <- read.csv('Corn_economic.csv')
Soy.economic      <- read.csv('Soy_economic.csv')


### Filter for relevant columns

Corn.economic     <- Corn.economic %>% dplyr::select(c(1, 2, 4, 5, 13:16, 29))

Soy.economic      <- Soy.economic %>% dplyr:: select(c(1, 2, 4, 5, 13, 27:29))


### Merge

Both.economic             <- merge(Corn.economic, Soy.economic, by = c('OverallANSI', 'Year'), all.x = TRUE, all.y = TRUE)
Both.economic             <- Both.economic[, -c(10)] ##drop USDA agregion y

colnames(Both.economic)[4:9]   <- paste0('CORN_', colnames(Both.economic)[4:9])
colnames(Both.economic)[10:14] <- paste0('SOY_', colnames(Both.economic)[10:14])

for(i in 1:nrow(Both.economic)) {

Both.economic$USDAagregion.x[i] <- ifelse(is.na(Both.economic$USDAagregion.x[i]), 
                                          unique(Both.economic$USDAagregion.x[
                                            which(Both.economic$OverallANSI == Both.economic$OverallANSI[i] & !is.na(Both.economic$USDAagregion.x))]),
                                          Both.economic$USDAagregion.x[i])
  
  
}

###################################################################################

### Deal with missing values

###################################################################################

### baseline values for imputation

### set NA values to 0, such that we get a lower median

Both.economic                                               <- fill_na(Both.economic)
Both.economic$TOTAL_Acreage                                 <- Both.economic$CORN_Acres_Planted.x + Both.economic$SOY_Acres_Planted.y

Both.economic$USDAagregion.x[is.na(Both.economic$USDAagregion.x)] <- 9 ### Soy Missippi

Economic.state <- Both.economic %>% group_by(.dots = c('USDAagregion.x','Year')) %>%
  summarise_all(median, na.rm = TRUE)


### Impute median values where missing

Both.economic$CORN_Acres_Planted.x[Both.economic$CORN_Acres_Planted.x == 0] <- -999999
Both.economic$SOY_Acres_Planted.y[Both.economic$SOY_Acres_Planted.y == 0]   <- -999999


for(i in 1:ncol(Both.economic)) {
  
  for(j in 1:nrow(Both.economic)) {
 
    Both.economic[j, i] <- ifelse(Both.economic[j, i] == 0, Economic.state[which(Economic.state$USDAagregion.x == Both.economic$USDAagregion.x[j] & Economic.state$Year == Both.economic$Year[j]), 
                                                                           which(colnames(Economic.state) == colnames(Both.economic)[i])][, 1], Both.economic[j, i])
     
  }
  
  print(i)
  
}

Both.economic$CORN_Acres_Planted.x[Both.economic$CORN_Acres_Planted.x == -999999] <- NA
Both.economic$SOY_Acres_Planted.y[Both.economic$SOY_Acres_Planted.y == -999999]   <- NA


###################################################################################

### Create cross commodity comparisons

###################################################################################

Both.economic$TOTAL_Margin_SoyCorn_Difference <- Both.economic$SOY_Margin_subsidyweighted.y - Both.economic$CORN_Margin_subsidyweighted.x



###################################################################################

### Calculate land use change

###################################################################################

### Interpolate missing values

Both.interp  <- lapply(unique(Both.economic$OverallANSI), rep, times =  28)
Both.interp  <- lapply(Both.interp, function(x) {data.frame(Year = 1990:2017, OverallANSI = x)})

Econ.split   <- split(Both.economic, Both.economic$OverallANSI)


for(i in 1:length(Both.interp)) {

Both.interp[[i]]  <- merge(Both.interp[[i]], Econ.split[[i]], on = c('Year', 'OverallANSI'), all.x = TRUE)

}

Both.interp.df <- rbind.fill(Both.interp)
test           <-  Both.interp.df 

test$SOY_Margin_subsidyweighted.y[test$SOY_Margin_subsidyweighted.y == -0]     <- NA
test$CORN_Margin_subsidyweighted.x[test$CORN_Margin_subsidyweighted.x == -0]   <- NA


for(i in 4:ncol(Both.interp.df)) {
  
  temp <- test[test$Year != 2017, i]
  
  test[test$Year != 2017, i] <- imputeTS::na.interpolation(temp)
  
  temp <- test[Both.interp.df$Year %in% c(2016, 2017), i]
  
  test[Both.interp.df$Year %in% c(2016, 2017), i] <- imputeTS::na.locf(temp)
  
  print(i)
  
}


for(i in 1:nrow(test)) {
  
  test$USDAagregion.x[i] <- ifelse(is.na(test$USDAagregion.x[i]), 
                                            unique(test$USDAagregion.x[
                                              which(test$OverallANSI == test$OverallANSI[i] & !is.na(test$USDAagregion.x))]),
                                   test$USDAagregion.x[i])
  
  
}


setwd('E:/Modules/Dissertation/Data/Processed Dat')
#
Both.economic <- read.csv('Economic_comparison.csv')

#########################################################################################

######### Overlap 2 analysis pipelines

#########################################################################################

Both.economic$SOY_Acres_Planted.y <- test$SOY_Acres_Planted.y
Both.economic$CORN_Acres_Planted.x<- test$CORN_Acres_Planted.x

##################################################################################################################

### Calculate Land Change

##################################################################################################################

Both.economic  <- split(Both.economic, Both.economic$OverallANSI)

Delta.Soy      <- data.frame(sapply(Both.economic, function(x) {diff(x$SOY_Acres_Planted.y)}))
Delta.Soy[28, ]<- NA
Delta.Soy      <- Delta.Soy[c(28, 1:27), ]
Delta.Soy      <- data.frame(apply(Delta.Soy, 2, function(x) {imputeTS::na.locf(x, option = 'nocb')}))

Delta.Corn     <- data.frame(sapply(Both.economic, function(x) {diff(x$CORN_Acres_Planted.x)}))
Delta.Corn[28, ]<- NA
Delta.Corn     <- Delta.Corn[c(28, 1:27), ]
Delta.Corn     <- data.frame(apply(Delta.Corn, 2, function(x) {imputeTS::na.locf(x, option = 'nocb')}))

Both.economic  <- rbind.fill(Both.economic)

colnames(Delta.Soy)      <- gsub('X', '', colnames(Delta.Soy))
colnames(Delta.Corn)     <- gsub('X', '', colnames(Delta.Corn))

Delta.Soy           <- gather(Delta.Soy)
Delta.Soy$Year      <- 1990:2017
colnames(Delta.Soy)[1] <- 'OverallANSI' 

Delta.Corn          <- gather(Delta.Corn)
Delta.Corn$Year     <- 1990:2017
colnames(Delta.Corn)[1]<- 'OverallANSI' 

Both.economic               <- merge(Both.economic, Delta.Soy, on = c('Year', 'OverallANSI'))
colnames(Both.economic)[24] <- 'DELTA_Soy2'

Both.economic               <- merge(Both.economic, Delta.Corn, on = c('Year', 'OverallANSI'))
colnames(Both.economic)[25] <- 'DELTA_Corn2'

Both.economic               <- Both.economic[ -c(17, 18)]



##################################################################################################################

### Integrate CRP and Pastured Cropland

##################################################################################################################

setwd('E:/Modules/Dissertation/Data/Model Ready/Commodity footprint')

CRP <- read.csv('CRP_Pasturedcrop.csv')


Both.economic <- merge(Both.economic, CRP[, 1:5], on = c('Year', 'OverallANSI'), all.x = TRUE)
Both.economic <- Both.economic[order(Both.economic$OverallANSI) , ]

colnames(Both.economic)[19:21] <- paste0('GENERAL_', colnames(Both.economic)[19:21])

### Calculate Delta Land use

Both.economic  <- split(Both.economic, Both.economic$OverallANSI)

Delta.Pasture  <- data.frame(sapply(Both.economic, function(x) {diff(x$GENERAL)}))
Delta.Soy[28, ]<- NA
Delta.Soy      <- Delta.Soy[c(28, 1:27), ]
Delta.Soy      <- data.frame(apply(Delta.Soy, 2, function(x) {imputeTS::na.locf(x, option = 'nocb')}))

Delta.Corn     <- data.frame(sapply(Both.economic, function(x) {diff(x$CORN_Acres_Planted.x)}))
Delta.Corn[28, ]<- NA
Delta.Corn     <- Delta.Corn[c(28, 1:27), ]
Delta.Corn     <- data.frame(apply(Delta.Corn, 2, function(x) {imputeTS::na.locf(x, option = 'nocb')}))

Both.economic  <- rbind.fill(Both.economic)

colnames(Delta.Soy)      <- gsub('X', '', colnames(Delta.Soy))
colnames(Delta.Corn)     <- gsub('X', '', colnames(Delta.Corn))

Delta.Soy           <- gather(Delta.Soy)
Delta.Soy$Year      <- 1990:2017
colnames(Delta.Soy)[1] <- 'OverallANSI' 

Delta.Corn          <- gather(Delta.Corn)
Delta.Corn$Year     <- 1990:2017
colnames(Delta.Corn)[1]<- 'OverallANSI' 

Both.economic               <- merge(Both.economic, Delta.Soy, on = c('Year', 'OverallANSI'))
colnames(Both.economic)[17] <- 'DELTA_Soy'

Both.economic               <- merge(Both.economic, Delta.Corn, on = c('Year', 'OverallANSI'))
colnames(Both.economic)[18] <- 'DELTA_Corn'


###################################################################################################################

### Substitute in Govt subsidy for insurance into Economic model

###################################################################################################################

setwd('E:/Modules/Dissertation/Data/Model Ready/Economic model')

Corn.economic     <- read.csv('Corn_economic.csv')
Soy.economic      <- read.csv('Soy_economic.csv')



Soy.Insure        <- data.frame(Insurance.dat %>% filter(Commodity.Name == 'SOYBEANS'))
Corn.Insure       <- data.frame(Insurance.dat %>% filter(Commodity.Name == 'CORN'))
Other.Insure      <- data.frame(Insurance.dat %>% filter(!Commodity.Name %in% c('SOYBEANS', 'CORN')))

Corn.economic     <- merge(Corn.economic, Corn.Insure[, c(1, 15, 22)], all.x = TRUE, by.x = c('Year', 'OverallANSI'), 
                           by.y = c('ï..Commodity.Year', 'FIPS'))

colnames(Corn.economic)[31] <- 'Insurance.subsidy'

Soy.economic      <- merge(Soy.economic, Soy.Insure[, c(1, 15, 22)], all.x = TRUE, by.x = c('Year', 'OverallANSI'), 
                           by.y = c('ï..Commodity.Year', 'FIPS'))

colnames(Soy.economic)[30] <- 'Insurance.subsidy'

######

# merge with economic data

######


setwd('E:/Modules/Dissertation/Data/Model Ready/Commodity footprint')

data      <- read.csv('Margin_to_Landuse.csv')

data      <- merge(data, Soy.Insure[, c(1, 15, 22)], all.x = TRUE, by.x = c('Year', 'OverallANSI'), 
                           by.y = c('ï..Commodity.Year', 'FIPS'))

data      <- merge(data, Corn.Insure[, c(1, 15, 22)], all.x = TRUE, by.x = c('Year', 'OverallANSI'), 
                   by.y = c('ï..Commodity.Year', 'FIPS'))

data      <- merge(data, Other.Insure[, c(1, 15, 22)], all.x = TRUE, by.x = c('Year', 'OverallANSI'), 
                   by.y = c('ï..Commodity.Year', 'FIPS'))

colnames(data)[32:33] <- c('SOY_Insurance.subsidy', 'CORN_Insurance.subsidy')
colnames(data)[34] <- c('OTHER_Insurance.subsidy')


#################################################################################################################

### Make Insurance subsidy data per acre

#################################################################################################################


setwd('E:/Modules/Dissertation/Data/Model Ready/Commodity footprint')

teg <- read.csv('Margin_to_Landuse2.csv')

### whoops, this has like 3,000 cases where insurance != 0 and Acres == 0.
#
#teg$SOY_Insurance.subsidy <- teg$SOY_Insurance.subsidy / teg$SOY_Acres_Planted.y
#
#

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Agricultural context')

Insurance.dat                <- read.csv('Insurance_processed.csv')
Insurance.dat                <- Insurance.dat[, -c(1)]

Soy.Insure        <- data.frame(Insurance.dat %>% filter(Commodity.Name == 'SOYBEANS'))
Corn.Insure       <- data.frame(Insurance.dat %>% filter(Commodity.Name == 'CORN'))
Other.Insure      <- data.frame(Insurance.dat %>% filter(!Commodity.Name %in% c('SOYBEANS', 'CORN')))

Corn.Insure       <- Corn.Insure[order(Corn.Insure$FIPS), ]
Soy.Insure       <- Soy.Insure[order(Soy.Insure$FIPS), ]

Corn.Insure$Quantity <- na.locf(Corn.Insure$Quantity)
Soy.Insure$Quantity  <- na.locf(Soy.Insure$Quantity)
Other.Insure$Quantity  <- na.locf(Other.Insure$Quantity)

teg     <- merge(test2, Soy.Insure[, c(1, 10, 22)], all.x = TRUE, by.x = c('Year', 'OverallANSI'), 
                   by.y = c('ï..Commodity.Year', 'FIPS'))

colnames(teg)[ncol(teg)] <- 'SOY_Acreageinsured2'

teg     <- merge(teg, Corn.Insure[, c(1, 10, 22)], all.x = TRUE, by.x = c('Year', 'OverallANSI'), 
                 by.y = c('ï..Commodity.Year', 'FIPS'))

colnames(teg)[ncol(teg)] <- 'CORN_Acreageinsured2'

teg     <- merge(teg, Other.Insure[, c(1, 10, 22)], all.x = TRUE, by.x = c('Year', 'OverallANSI'), 
                 by.y = c('ï..Commodity.Year', 'FIPS'))

colnames(teg)[ncol(teg)] <- 'OTHER_Acreageinsured2'

teg     <- teg[, -c(34:35)]

#fill_na                   <- colwise(function(x) {ifelse(is.na(x), 0, x)})
#teg[, c(32:34, 36:37)] <- fill_na(teg[, c(32:34, 36:37)])


teg <- teg[order(teg$OverallANSI), ]

#teg$SOY_Acreageinsured2 <- imputeTS::na.interpolation(teg$SOY_Acreageinsured2)

teg <- split(teg, teg$OverallANSI)

for(i in 1:length(teg)) {
    
  try(
  
    teg[[i]]$SOY_Acreageinsured2 <- imputeTS::na.interpolation(teg[[i]]$SOY_Acreageinsured2)
    
  )
  
  try(
    teg[[i]]$CORN_Acreageinsured2 <- imputeTS::na.interpolation(teg[[i]]$CORN_Acreageinsured2)
    
  )
  
  try(
    teg[[i]]$OTHER_Acreageinsured2 <- imputeTS::na.interpolation(teg[[i]]$OTHER_Acreageinsured2)
    
  )
    
}
  
teg.test <- rbind.fill(teg)
  
  
teg.test$SOY_Acreageinsured2   <-   teg.test$SOY_Acreageinsured2 / teg.test$SOY_Acres_Planted.y
teg.test$SOY_Acreageinsured2   <- ifelse(teg.test$SOY_Acreageinsured2 > 1, 1, teg.test$SOY_Acreageinsured2)

teg.test$CORN_Acreageinsured2  <-   teg.test$CORN_Acreageinsured2 / teg.test$CORN_Acres_Planted.x
teg.test$CORN_Acreageinsured2  <- ifelse(teg.test$CORN_Acreageinsured2 > 1, 1, teg.test$CORN_Acreageinsured2)

teg.test$OTHER_Acreageinsured2 <-   teg.test$OTHER_Acreageinsured2 / teg.test$OTHER_TOTAL
teg.test$OTHER_Acreageinsured2 <- ifelse(teg.test$OTHER_Acreageinsured2 > 1, 1, teg.test$OTHER_Acreageinsured2)

### Agregion median impute

teg.test.State <- teg.test %>% group_by(.dots = c('Year', 'State')) %>% 
  summarise(SOY = mean(SOY_Acreageinsured2, na.rm = TRUE),
            CORN = mean(CORN_Acreageinsured2, na.rm = TRUE),
            OTHER = mean(OTHER_Acreageinsured2, na.rm = TRUE))


for (i in 38:40) {
  
  for(j in 1:length(teg.test[, i])) {
  
    if (is.na(teg.test[j,i])) {
    
  teg.test[j, i] <- as.numeric(teg.test.State[which(teg.test$State[j] == teg.test.State$State &
                                           teg.test$Year[j] == teg.test.State$Year), i-35])
  
    }
  
  }
  
  print(i)
  
}

#### RANDOM TIDYING

teg.test$OTHER_Insurance.subsidy <- teg.test$OTHER_Insurance.subsidy/teg.test$OTHER_TOTAL

teg.test$TOTAL_SOYCRP_Difference  <- teg.test$SOY_Margin_subsidyweighted.y - teg.test$GENERAL_Payment
teg.test$TOTAL_CORNCRP_Difference <- teg.test$CORN_Margin_subsidyweighted.x - teg.test$GENERAL_Payment

### drop cols

teg.test <- teg.test[, -c(30:32)]

#----------------------------------------------------------------------------------------------------------

#test <- list()
#
#
#for(i in 1:length(teg)) {
#
#  for(j in 1:length(teg[[i]])){
#  
#  dat <- teg[[i]][[j]] %>% dplyr::filter(!(SOY_Acres_Planted.y == 0 & SOY_Acreageinsured != 0))
#  
#  dat.pred <- teg[[i]][[j]] %>% dplyr::filter((SOY_Acres_Planted.y == 0 & SOY_Acreageinsured != 0))
#  
#  mod <- lm(SOY_Acres_Planted.y ~ SOY_Acreageinsured, data = dat)

  test <- c(test, summary(mod)$r.squared)
  
  try(
  
  teg[[i]][[j]][teg[[i]][[j]]$SOY_Acres_Planted.y == 0 & teg[[i]][[j]]$SOY_Insurance.subsidy != 0, ]$SOY_Acres_Planted.y <- predict.lm(mod, dat.pred)
  
  )
  
  try(
    
    teg[[i]][[j]][teg[[i]][[j]]$SOY_Acres_Planted.y == 0 & teg[[i]][[j]]$SOY_Insurance.subsidy != 0, ]$SOY_Acres_Planted.y <- median(dat$SOY_Acres_Planted.y, na.rm = TRUE)
    
  )
  
  }
  
  
  
}

teg2 <- rbind.fill(lapply(teg, rbind.fill))



### Corn


nlm.list <- list()
test     <- list()
nmedians <- list()

for(i in 1:length(teg)) {
  
  for(j in 1:length(teg[[i]])){
    
    dat <- teg[[i]][[j]] %>% dplyr::filter(!(CORN_Acres_Planted.x == 0 & CORN_Acreageinsured != 0))
    
    dat.pred <- teg[[i]][[j]] %>% dplyr::filter((CORN_Acres_Planted.x == 0 & CORN_Acreageinsured != 0))
    
    
    mod <- lm(CORN_Acres_Planted.x ~ CORN_Acreageinsured, data = dat)
    
    test <- c(test, summary(mod)$r.squared)
    
    if(summary(mod)$r.squared > 0.5) {
    
      try(
      
      teg[[i]][[j]][teg[[i]][[j]]$CORN_Acres_Planted.x == 0 & teg[[i]][[j]]$CORN_Acreageinsured != 0, ]$CORN_Acres_Planted.x <- predict.lm(mod, dat.pred)
      
      )
      
      try(
      nlm.list <- c(nlm.list, nrow(dat.pred))
      )
      
    } else {
      
      try(
      
      teg[[i]][[j]][teg[[i]][[j]]$CORN_Acres_Planted.x == 0 & teg[[i]][[j]]$CORN_Acreageinsured != 0, ]$CORN_Acres_Planted.x <- mean(dat$CORN_Acres_Planted.x, na.rm = TRUE)
      
      )
      
      try(
        
        nmedians <- c(nmedians, nrow(dat.pred))
      )
      
    }
      
      
      
  }
  
  

}

for(i in 1:length(teg)) {
  
  for(j in 1:length(teg[[i]])){
    
    dat <- teg[[i]][[j]] %>% dplyr::filter(!(CORN_Acres_Planted.x == 0 & CORN_Acreageinsured != 0))

try(
  
  teg[[i]][[j]][teg[[i]][[j]]$CORN_Acres_Planted.x == 0 & teg[[i]][[j]]$CORN_Acreageinsured != 0, ]$CORN_Acres_Planted.x <- mean(dat$CORN_Acres_Planted.x, na.rm = TRUE)
  
)
    
  }
}



dat.clean <- rbind.fill(lapply(teg, rbind.fill))

CORN.Insurance.test <- dat.clean$CORN_Insurance.subsidy / dat.clean$CORN_Acres_Planted.x
temp                <- dat.clean[which(is.infinite(CORN.Insurance.test)),]

dat.clean[which(is.infinite(CORN.Insurance.test)),]$CORN_Acres_Planted.x <-  (temp$SOY_Acreageinsured / temp$SOY_Acres_Planted.y) * temp$CORN_Acreageinsured

dat.clean$SOY_Insurance.subsidy  <- dat.clean$SOY_Insurance.subsidy / dat.clean$SOY_Acres_Planted.y
dat.clean$CORN_Insurance.subsidy <- dat.clean$CORN_Insurance.subsidy / dat.clean$CORN_Acres_Planted.x

dat.clean$SOY_Acreageinsured     <- dat.clean$SOY_Acreageinsured / dat.clean$SOY_Acres_Planted.y
dat.clean$SOY_Acreageinsured     <- ifelse(dat.clean$SOY_Acreageinsured > 1, 1, dat.clean$SOY_Acreageinsured)

dat.clean$CORN_Acreageinsured     <- dat.clean$CORN_Acreageinsured / dat.clean$CORN_Acres_Planted.x
dat.clean$CORN_Acreageinsured     <- ifelse(dat.clean$CORN_Acreageinsured > 1, 1, dat.clean$CORN_Acreageinsured)

#############################################################################################################

### REDO Delta

#############################################################################################################

Both.economic  <- split(dat.clean, dat.clean$OverallANSI)

Delta.Soy      <- data.frame(sapply(Both.economic, function(x) {diff(x$SOY_Acres_Planted.y)}))
Delta.Soy[28, ]<- NA
Delta.Soy      <- Delta.Soy[c(28, 1:27), ]
Delta.Soy      <- data.frame(apply(Delta.Soy, 2, function(x) {imputeTS::na.locf(x, option = 'nocb')}))

Delta.Corn     <- data.frame(sapply(Both.economic, function(x) {diff(x$CORN_Acres_Planted.x)}))
Delta.Corn[28, ]<- NA
Delta.Corn     <- Delta.Corn[c(28, 1:27), ]
Delta.Corn     <- data.frame(apply(Delta.Corn, 2, function(x) {imputeTS::na.locf(x, option = 'nocb')}))

Both.economic  <- rbind.fill(Both.economic)

colnames(Delta.Soy)      <- gsub('X', '', colnames(Delta.Soy))
colnames(Delta.Corn)     <- gsub('X', '', colnames(Delta.Corn))

Delta.Soy           <- gather(Delta.Soy)
Delta.Soy$Year      <- 1990:2017
colnames(Delta.Soy)[1] <- 'OverallANSI' 

Delta.Corn          <- gather(Delta.Corn)
Delta.Corn$Year     <- 1990:2017
colnames(Delta.Corn)[1]<- 'OverallANSI' 

Both.economic               <- merge(Both.economic, Delta.Soy, on = c('Year', 'OverallANSI'))
Both.economic               <- Both.economic[, -c(18)]
colnames(Both.economic)[37] <- 'DELTA_Soy'

Both.economic               <- merge(Both.economic, Delta.Corn, on = c('Year', 'OverallANSI'))
Both.economic               <- Both.economic[, -c(18)]
colnames(Both.economic)[37] <- 'DELTA_Corn'

Both.economic$CORN_Insurance.subsidy[Both.economic$CORN_Insurance.subsidy > 1000] <- 1000
Both.economic$SOY_Insurance.subsidy[Both.economic$SOY_Insurance.subsidy > 1000] <- 1000

Both.economic$DELTA_Corn[Both.economic$DELTA_Corn < -1000000] <- -1000000

dat.clean <- Both.economic

#setwd('E:/Modules/Dissertation/Data/Model Ready/Commodity footprint')
#write.csv(Both.economic, 'Margin_to_Landuse3.csv', row.names = FALSE)

### CONVERT CRP PAYMENT TO PER ACRE

dat.clean$GENERAL_Payment          <- dat.clean$GENERAL_Payment / dat.clean$GENERAL_CRP_Acreage
teg.test$TOTAL_SOYCRP_Difference  <- teg.test$SOY_Margin_subsidyweighted.y - dat.clean$GENERAL_Payment
teg.test$TOTAL_CORNCRP_Difference <- teg.test$CORN_Margin_subsidyweighted.x - dat.clean$GENERAL_Payment



setwd('E:/Modules/Dissertation/Data/Model Ready/Commodity footprint')
write.csv(dat.clean, 'Margin_to_LanduseFINAL.csv', row.names= FALSE)


##################################################################################################

### Calculate Delta for total land use

##################################################################################################

setwd('E:/Modules/Dissertation/Data/Model Ready/Commodity footprint')

data  <- read.csv('Margin_to_Landuse2.csv')

data  <- split(data, data$OverallANSI)


Delta.total      <- data.frame(sapply(data, function(x) {diff(x$OTHER_TOTAL)}))
Delta.total[28, ]<- NA
Delta.total      <- Delta.total[c(28, 1:27), ]
Delta.total      <- data.frame(apply(Delta.total, 2, function(x) {imputeTS::na.locf(x, option = 'nocb')}))


data  <- rbind.fill(data)

colnames(Delta.total)      <- gsub('X', '', colnames(Delta.total))

Delta.total          <- gather(Delta.total)
Delta.total$Year      <- 1990:2017
colnames(Delta.total)[1] <- 'OverallANSI' 

data               <- merge(data, Delta.total, on = c('Year', 'OverallANSI'))
colnames(data)[35] <- 'DELTA_Total'




#############################################################################################################

### ********************************************************************************************************


### EXPECTATION


### ********************************************************************************************************

#############################################################################################################

setwd('E:/Modules/Dissertation/Data/Model Ready/Commodity footprint')

data      <- read.csv('Margin_to_LanduseFINAL.csv')

data      <- data %>% dplyr::select(1:3)

setwd('E:/Modules/Dissertation/Data/Model Ready/Economic model')

Corn      <- read.csv('Corn_economic.csv')
Corn      <- Corn %>% dplyr::select(1, 2, 3, 8, 12)

Soy       <- read.csv('Soy_economic.csv')
Soy       <- Soy %>% dplyr::select(1, 2,3, 8, 12)

colnames(Soy)[5] <- 'Soy.Yield'

### merge yield & price data with Land

data      <- merge(data, Corn, all.x = TRUE)
data      <- merge(data, Soy, all.x = TRUE)
data      <- data[order(data$OverallANSI), ]

data      <- split(data, data$OverallANSI)

##################

### Calculate EV - rolling window of 3 years + possible lm

##################


### Linear Extrapolation

for(i in 1:length(data)) {

  data[[i]][, 4:7] <- apply(data[[i]][, 4:7], 2, function(x) {
    
    if(length(which(is.na(x))) < 26) {
      
      imputeTS::na.interpolation(x)
      
    } else
      
      x
    
  
  })
  
}

### Function to calculate moving window
  
slideFunct <- function(dat, window, step){
  total <- length(dat)
  spots <- seq(from=1 + window, to=(total), by=step)
  result <- vector(length = length(spots))
  for(i in 4:28){
    result[c(i-window)] <- mean(dat[c((i-window):(i - 1))], na.rm = TRUE)
  }
  return(result)
}

#teg <- data[[345]]

#total <- length(teg$Yield)
#spots <- seq(from=1 + 3, to=(total), by=1)
#result <- vector(length = length(spots))
#
#for(i in 4:28){
#  result[i-window] <- mean(teg$Yield[c((i-window):(i - 1))])
#}


### calculate

Soy.Yield.rolling <- lapply(data, function(x) {slideFunct(x$Soy.Yield, window = 3, step = 1)})
Soy.Yield.rolling <- lapply(Soy.Yield.rolling, function(x) {c(rep(x[1], times = 3), x)})

Soy.Price.rolling <- lapply(data, function(x) {slideFunct(x$SOYBEANS...PRICE.RECEIVED..MEASURED.IN.....BU, 3, 1)})
Soy.Price.rolling <- lapply(Soy.Price.rolling, function(x) {c(rep(x[1], times = 3), x)})

Corn.Yield.rolling <- lapply(data, function(x) {slideFunct(x$Yield, 3, 1)})
Corn.Yield.rolling <- lapply(Corn.Yield.rolling, function(x) {c(rep(x[1], times = 3), x)})

Corn.Price.rolling <- lapply(data, function(x) {slideFunct(x$CORN..GRAIN...PRICE.RECEIVED..MEASURED.IN.....BU, 3, 1)})
Corn.Price.rolling <- lapply(Corn.Price.rolling, function(x) {c(rep(x[1], times = 3), x)})
  
for(i in 1:length(data)) {
  
  data[[i]]$Soy.Yield.Expectation <- Soy.Yield.rolling[[i]]
  data[[i]]$Soy.Price.Expectation <- Soy.Price.rolling[[i]]
  data[[i]]$Corn.Yield.Expectation<- Corn.Yield.rolling[[i]]
  data[[i]]$Corn.Price.Expectation<- Corn.Price.rolling[[i]]
  
  print(i)
  
}



data.test <- rbind.fill(data)


### create expected values where we have NA

States <- data.test %>% group_by(.dots = c('Year', 'State')) %>% summarise_all(median)

fill_na   <- colwise(function(x) {ifelse(is.na(x) , 0, x)})
data.test[, 4:ncol(data.test)] <- fill_na(data.test[, 4:ncol(data.test)]) 


for(i in 4:ncol(data.test)) {
  
  col <- data.frame(States)[, i]
  
  for(j in 1:length(data.test[, i])) {
  
    if(data.test[j, i] == 0) {
    
  data.test[j, i] <- col[which(States$State == data.test$State[j] & States$Year == data.test$Year[j])]
  
    }
    
  }
  
  print(i)
  
}

############################################################################################

### Use new expectations to create expected gross value of production

############################################################################################

### Original data

setwd('E:/Modules/Dissertation/Data/Model Ready/Economic model')

Corn      <- read.csv('Corn_economic.csv')

Soy       <- read.csv('Soy_economic.csv')

#setwd('E:/Modules/Dissertation/Data/Model Ready/Commodity footprint')

#test2      <- read.csv('Margin_to_LanduseFINAL.csv')

#write.csv(teg.test, 'Margin_to_LanduseCORRECTED.csv', row.names = FALSE)


###########################################################################################

Corn <- merge(Corn, data.test[, c(1, 2, 10, 11)], all.x = TRUE)
Soy  <- merge(Soy, data.test[, c(1, 2, 8, 9)], all.x = TRUE)

Soy$EXPECTED.GROSS <- Soy$Soy.Yield.Expectation * Soy$Soy.Price.Expectation
Corn$EXPECTED.GROSS<- Corn$Corn.Price.Expectation * Corn$Corn.Yield.Expectation

Soy$EXPECTED.WEIGHTED <- Soy$EXPECTED.GROSS * (Soy$SOYBEANS...ACRES.HARVESTED / Soy$Acres_Planted)
Corn$EXPECTED.WEIGHTED <- Corn$EXPECTED.GROSS * (Corn$CORN..GRAIN...ACRES.HARVESTED / Corn$Acres_Planted)

Soy$EXPECTED.MARGIN.SUBSIDY <- (Soy$EXPECTED.WEIGHTED + Soy$Generic_subsidy + Soy$Insurance + Soy$ACRO_CO.Subsidy) -
                                      (Soy$Overhead + Soy$Seed + Soy$Fertiliser + Soy$Fuel + Soy$Chemicals + Soy$Customservices +
                                         Soy$Repairs)

Corn$EXPECTED.MARGIN.SUBSIDY <- (Corn$EXPECTED.WEIGHTED + Corn$Generic_subsidy + Corn$Insurance + Corn$ACRO_CO.Subsidy + Corn$Old_Commodity_Payment) -
  (Corn$Overhead + Corn$Seed + Corn$Fertiliser + Corn$Fuel + Corn$Chemicals + Corn$Customservices +
     Corn$Repairs)

data.test <- merge(data.test, Corn[, c(1, 2, 3, 4, 35)], all.x = TRUE)
colnames(data.test)[13] <- 'CORN_EXPECTED.MARGIN.SUBSIDY'

data.test <- merge(data.test, Soy[, c(1, 2, 3, 4, 34)], all.x = TRUE)
colnames(data.test)[14] <- 'SOY_EXPECTED.MARGIN.SUBSIDY'

data.test.list <- split(data.test, data.test$OverallANSI)

for(i in 1:length(data.test.list)) {
  
  try(
  
  
    data.test.list[[i]]$'CORN_EXPECTED.MARGIN.SUBSIDY' <- imputeTS::na.interpolation(data.test.list[[i]]$'CORN_EXPECTED.MARGIN.SUBSIDY') 
  
  )
  
  try(
    data.test.list[[i]]$'SOY_EXPECTED.MARGIN.SUBSIDY'  <- imputeTS::na.interpolation(data.test.list[[i]]$'SOY_EXPECTED.MARGIN.SUBSIDY')
  )
    
    
  }


data.test <- rbind.fill(data.test.list)

data.test[, 13:14] <- fill_na(data.test[, 13:14])

expectations <- data.test %>% group_by(.dots = c('Year', 'State')) %>% summarise(Corn = mean(CORN_EXPECTED.MARGIN.SUBSIDY, na.rm = TRUE), 
                                                                                        Soy = mean(SOY_EXPECTED.MARGIN.SUBSIDY, na.rm = TRUE))


expectations <- as.data.frame(expectations)

for(i in 13:14) {
  
  for(j in 1:length(data.test[, i])) {
    
    
    if(data.test[j, i] == 0) {
      
      data.test[j, i] <- as.numeric(expectations[which(expectations$Year == data.test$Year[j] & expectations$State == data.test$State[j]), (i - 10)])
    
    }
  }
  
}

setwd('E:/Modules/Dissertation/Data/Model Ready/Commodity footprint')

test2      <- read.csv('Margin_to_LanduseCORRECTED.csv')

test2      <- merge(test2, data.test[, c(1, 2, 3, 4, 13, 14)], all.x = TRUE)

test2$EXPECTED.SoyCorn_difference      <- test2$SOY_EXPECTED.MARGIN.SUBSIDY - test2$CORN_EXPECTED.MARGIN.SUBSIDY
test2$EXPECTED.SOYCRP_difference       <- test2$SOY_EXPECTED.MARGIN.SUBSIDY - test2$GENERAL_Payment
test2$EXPECTED.CORNCRP_difference      <- test2$CORN_EXPECTED.MARGIN.SUBSIDY - test2$GENERAL_Payment

test2 <- test2[, -c(38)]

test2 <- test2[order(test2$OverallANSI), ]

test2.list <- split(test2, test2$OverallANSI) 

#################### PCT CHANGE


for(i in 1:length(test2.list)) {


    test2.list[[i]]$DELTA_SOY_PCT <- test2.list[[i]]$DELTA_Soy / test2.list[[i]]$SOY_Acres_Planted.y
  
    test2.list[[i]]$DELTA_CORN_PCT<- test2.list[[i]]$DELTA_Corn / test2.list[[i]]$CORN_Acres_Planted.x

    test2.list[[i]]$DELTA_CRP_PCT <- test2.list[[i]]$DELTA_CRP / test2.list[[i]]$GENERAL_CRP_Acreage
  
    test2.list[[i]]$DELTA_OTHER_PCT <- test2.list[[i]]$DELTA_Total / test2.list[[i]]$OTHER_TOTAL
    
  
}

test2 <- rbind.fill(test2.list)


write.csv(test2, 'Margin_to_LanduseCORRECTED.csv', row.names = FALSE)





test2$CORN_Acres_Planted.x <- Both.economic$CORN_Acres_Planted.x
test2$SOY_Acres_Planted.y  <- Both.economic$SOY_Acres_Planted.y
test2$DELTA_Corn           <- Both.economic$DELTA_Corn2
test2$DELTA_Soy            <- Both.economic$DELTA_Soy2

test2                      <- test2[order(test2$OverallANSI), ]
test2$OTHER_Insurance.subsidy <- test2$OTHER_Insurance.subsidy / test2$TOTAL_Acreage
  
#lapply(Soy, function(x) {})

#eg <- data.test %>% filter(State == 'NEBRASKA') 
#
#summary(teg$Soy.Yield)
#
#teg$Soy.Yield <- ifelse(is.na(teg$Soy.Yield), 0, teg$Soy.Yield)
#median(teg$Soy.Yield)

############################################################################################################

### CREATE KEY FOR ONLY COUNTIES WITH COMPLETE DATA

############################################################################################################


setwd('E:/Modules/Dissertation/Data/Model Ready/Economic model')

Soy   <- read.csv('Soy_economic.csv')
Corn  <- read.csv('Corn_economic.csv')

Soy.countykey    <- table(Soy$OverallANSI)
Corn.countykey   <- table(Corn$OverallANSI)

Soy.countykey    <- Soy.countykey[as.numeric(as.character(Soy.countykey)) > 26]
Corn.countykey   <- Corn.countykey[as.numeric(as.character(Corn.countykey)) > 26]

County.key       <- Soy.countykey[names(Soy.countykey) %in% names(Corn.countykey)]

Corn.key         <- unique(Corn$OverallANSI[as.character(Corn$OverallANSI) %in% names(County.key)])
Soy.key          <- unique(Soy$OverallANSI[Soy$OverallANSI %in% names(County.key)])

setwd('E:/Modules/Dissertation/Data/Model Ready')
#writeLines(as.character(names(County.key)), 'Parsimonious_Counties.txt')



############################################################################################################

### END OF MARGIN TO LAND USE

############################################################################################################




##############################################################################################################

#*************************************************************************************************************

### Economic Data

#*************************************************************************************************************
##############################################################################################################


setwd('E:/Modules/Dissertation/Data/Model Ready/Economic model')

Soy                    <- read.csv('Soy_economic.csv')
Soy                    <- Soy %>% group_by(Year) %>% 
                            summarise(Price = mean(SOYBEANS...PRICE.RECEIVED..MEASURED.IN.....BU, na.rm = TRUE), 
                                      Yield = mean(Yield, na.rm = TRUE), 
                                      Production = sum(SOYBEANS...PRODUCTION..MEASURED.IN.BU, na.rm = TRUE))
                                      

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Soy')

Biodiesel.Econ         <- read.csv("Biodiesel Economics.csv", skip = 3)
Biodiesel.Production   <- read.csv('Soy_Biofuel_Production.csv')
Soy.Export             <- read.csv('Soy_Export_Beans.csv', skip = 3)
Brazil.China.Soy       <- read.csv('Brazil_China_Soy.csv')
China_all_Soy          <- read.csv('China_all_Soy.csv')



setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Agricultural context')

Biofuel.subsidies      <- read.csv('Biofuel subsidies.csv')
Biofuel.subsidies      <- data.frame(apply(Biofuel.subsidies, 2, function(x) {ifelse(is.na(x), 0, x)}))

########################################################

### 1) Data Processing

########################################################

### Biodiesel Price

Biodiesel.Econ         <- Biodiesel.Econ %>% dplyr::select(1:3)
Biodiesel.Econ         <- Biodiesel.Econ %>% filter(Month.yr != '')

Biodiesel.Econ$Month.yr<- as.yearmon(2007 + seq(0, 145)/12)
Biodiesel.Econ$Year    <- as.integer(gsub('[a-zA-Z]', '', Biodiesel.Econ$Month.yr))

Biodiesel.Econ         <- Biodiesel.Econ %>% group_by(Year) %>% summarise_at(2:3, mean, na.rm = TRUE)

### Biodiesel Production

colnames(Biodiesel.Production)[1]     <-  'Year' 

Biodiesel.Production$Marketing.Year   <-  substr(Biodiesel.Production$Marketing.Year, 1, 5)
Biodiesel.Production$Marketing.Year   <-  gsub('/', '', Biodiesel.Production$Marketing.Year)
Biodiesel.Production$Marketing.Year   <-  gsub(' ', '', Biodiesel.Production$Marketing.Year)


### Soy Exports

colnames(Soy.Export)                  <- gsub('X', '', colnames(Soy.Export))
colnames(Soy.Export)                  <- gsub("\\..*","",colnames(Soy.Export))
Countries                             <- Soy.Export[, 1]

Soy.Export                            <- Soy.Export[, c(colnames(Soy.Export) %in% 1990:2017)]
Soy.Export$Country                    <- Countries
Soy.Export                            <- Soy.Export[, c(ncol(Soy.Export), 1:(ncol(Soy.Export)-1))]

#colnames(Soy.Export)                  <- paste0(Soy.Export[2, ], '_', colnames(Soy.Export))
Qty                                   <- t(data.frame(Soy.Export[1, ]))
Qty                                   <- Qty[, 1]
Soy.Export                            <- Soy.Export[, c(Qty == 'Qty')]

Soy.Export$Country                    <- Countries
Soy.Export                            <- Soy.Export[, c(ncol(Soy.Export), 1:(ncol(Soy.Export)-1))]

Soy.Export                            <- Soy.Export[-1, ]
Soy.Export[, 2:ncol(Soy.Export)]      <- apply(Soy.Export[, 2:ncol(Soy.Export)],2, function(x) {
                                              as.numeric(as.character(x))})
Soy.Export[133, ]                     <- NA
Soy.Export[133, 2:ncol(Soy.Export)]   <- apply(Soy.Export[, 2:ncol(Soy.Export)], 2, 
                                                          function(x) {sum(as.numeric(as.character((x))), na.rm = TRUE)})
Soy.Export[133, 1]                    <- 'Annual Total'


################

# China / Brazil

################

China_all_Soy                         <- China_all_Soy %>% dplyr::select(2, 28)
China_all_Soy$Year                    <- as.numeric(China_all_Soy$Year)
China_all_Soy                         <- China_all_Soy[order(China_all_Soy$Year), ]

China_all_Soy.Extrapolate              <- data.frame(Year = c(1990:2017))
China_all_Soy.Extrapolate              <- merge(China_all_Soy.Extrapolate, China_all_Soy, by = 'Year', all.x = TRUE)
China_all_Soy.Extrapolate$Alt.Qty.Unit[1:2]              <- 0
China_all_Soy.Extrapolate$Alt.Qty.Unit <- imputeTS::na.interpolation(China_all_Soy.Extrapolate)


################

# China / World

################

Brazil.China.Soy                      <- Brazil.China.Soy %>% filter(Reporter == 'China' & Partner == 'Brazil' & Trade.Flow == 'Import')
Brazil.China.Soy                      <- Brazil.China.Soy %>% dplyr::select(2, 28)
Brazil.China.Soy$Year                 <- as.numeric(Brazil.China.Soy$Year)
Brazil.China.Soy                      <- Brazil.China.Soy[order(Brazil.China.Soy$Year), ]

Brazil.China.Extrapolate              <- data.frame(Year = c(1990:2017))
Brazil.China.Extrapolate              <- merge(Brazil.China.Extrapolate , Brazil.China.Soy, by = 'Year', all.x = TRUE)
Brazil.China.Extrapolate$Alt.Qty.Unit[1:2]              <- 0
Brazil.China.Extrapolate$Alt.Qty.Unit <- imputeTS::na.interpolation(Brazil.China.Extrapolate$Alt.Qty.Unit)


#############################################################################################################

### 2) Merge

#############################################################################################################

Soy                                    <- merge(Soy, Brazil.China.Extrapolate, all.x = TRUE)
colnames(Soy)[4]                       <- 'Brazil.China.Exports'
Soy$Brazil.China.Exports               <- Soy$Brazil.China.Exports / 1000

US.China                               <- as.numeric(Soy.Export[1, 2:ncol(Soy.Export)])
Soy$US.China.Exports                   <- US.China

Soy                                    <- merge(Soy, China_all_Soy.Extrapolate, all.x = TRUE)
colnames(Soy)[6]                       <- 'Total.China.Imports'
Soy$Total.China.Imports                <- Soy$Total.China.Imports$Alt.Qty.Unit
Soy$Total.China.Imports                <- Soy$Total.China.Imports / 1000
Soy                                    <- Soy[, c(1:6)]


colnames(Biodiesel.Production)[1]      <- 'Year' 
Soy                                    <- merge(Soy, Biodiesel.Production[, c(1, 3, 6)], all.x = TRUE)
colnames(Soy)[7]                       <- 'US.Biodiesel.Production'
colnames(Soy)[8]                       <- 'US.Biodiesel.Consumption'

Soy$US.Biodiesel.Production            <- ifelse(is.na(Soy$US.Biodiesel.Production), 0, Soy$US.Biodiesel.Production)
Soy$US.Biodiesel.Consumption           <- ifelse(is.na(Soy$US.Biodiesel.Consumption), 0, Soy$US.Biodiesel.Consumption)

colnames(Biofuel.subsidies)[1]         <- 'Year' 
Soy2                                   <- merge(Soy, Biofuel.subsidies, all.x = TRUE, by = 'Year')

Soy2$US.Production                     <- Soy$Production
Soy2                                   <- Soy2[, c(1:3, 18, 4:17)]

####################################################################################################

#setwd('E:/Modules/Dissertation/Data/Model Ready/Price')
#Soy <- read.csv('Soy_price.csv')


#### Add biodiesel Price

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Soy')

Historical                             <- read.csv('Historical_Diesel.csv')
colnames(Historical)                   <- gsub('X', '', colnames(Historical))
Historical.dat                         <- tidyr::gather(Historical[, c(5:ncol(Historical))])
Historical.dat$value                   <- imputeTS::na.interpolation(Historical.dat$value)
Historical.dat$value                   <- Historical.dat$value * 3.78541
Historical.dat                         <- Historical.dat %>% filter(key > 1989 & key < 2007)

Diesel.econ                            <-  read.csv('Biodiesel_Price.csv', skip = 3)      
Diesel.econ$X                          <-  as.numeric(paste0('20', substr(as.character(Diesel.econ$X), nchar(as.character(Diesel.econ$X))-1, 
                                                               nchar(as.character(Diesel.econ$X)))))  

Diesel.econ                            <- Diesel.econ %>% group_by(X) %>% 
                                            summarise_all(mean, na.rm = TRUE) %>% filter(X > 2006) %>% dplyr::select(1:3)

colnames(Diesel.econ)                  <- c('Year', 'Biodiesel.Price', 'Diesel.Price')

Price.key                              <- c(1.96, 1.88, 1.82, 1.77, 1.72, 1.68, 
                                            1.63, 1.59, 1.57, 1.53, 1.48, 1.44,
                                            1.42, 1.39, 1.35, 1.31, 1.27, rep(NA, times = 11))

Soy                                    <- merge(Soy, Diesel.econ, all.x = TRUE)

Soy$Biodiesel.Price                    <- ifelse(is.na(Soy$Biodiesel.Price), 0, Soy$Biodiesel.Price)

Soy$Diesel.Price[is.na(Soy$Diesel.Price)] <- Historical.dat$value * Price.key[1:17]

setwd('E:/Modules/Dissertation/Data/Model Ready/Price')

write.csv(Soy, 'Soy_price.csv', row.names = FALSE)

#####################################################################################################

### Corn

#####################################################################################################

setwd('E:/Modules/Dissertation/Data/Model Ready')

Corn                    <- read.csv('Corn_economic.csv')
Corn                    <- Corn %>% group_by(Year) %>% 
  summarise(Price = mean(CORN..GRAIN...PRICE.RECEIVED..MEASURED.IN.....BU, na.rm = TRUE), 
            Yield = mean(Yield, na.rm = TRUE), 
            Production = sum(CORN..GRAIN...PRODUCTION..MEASURED.IN.BU, na.rm = TRUE))


setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Agricultural context')

Biofuel.subsidies               <- read.csv('Biofuel subsidies.csv')
Biofuel.subsidies               <- data.frame(apply(Biofuel.subsidies, 2, function(x) {ifelse(is.na(x), 0, x)}))
colnames(Biofuel.subsidies)[1]  <- 'Year'

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Corn')

Ethanol.econ           <- read.csv('Corn Ethanol economics.csv', skip = 2)
Ethanol.prod           <- read.csv('Ethanol Production.csv', skip = 3)

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Infrastructure')

Ethanol.infrastructure <- read.csv('Corn Infrastructure USA.csv')


##################################################################################################

### Process data

##################################################################################################

### Ethanol economics

Ethanol.econ                                       <- Ethanol.econ [-1, ]
Ethanol.econ$Marketing.year.and.quarter1[Ethanol.econ$Marketing.year.and.quarter1 == '']           <- NA
Ethanol.econ$Marketing.year.and.quarter1           <- as.numeric(substr(Ethanol.econ$Marketing.year.and.quarter1, 1, 4))

Ethanol.econ$Marketing.year.and.quarter1           <- imputeTS::na.locf(Ethanol.econ$Marketing.year.and.quarter1)

Ethanol.econ                                       <- Ethanol.econ %>% filter(X == 'Avg. Sep-Aug')
Ethanol.econ                                       <- Ethanol.econ %>% dplyr::select(1:8)
Ethanol.econ                                       <- Ethanol.econ %>% dplyr::select(c(1,4,5,6))
Ethanol.econ                                       <- Ethanol.econ %>% filter(Marketing.year.and.quarter1 > 1989)
colnames(Ethanol.econ)[1]                          <- 'Year'

### Ethanol production

Ethanol.prod                                       <- Ethanol.prod[-c(1), ]
Ethanol.prod$Marketing.year.and.quarter1[Ethanol.prod$Marketing.year.and.quarter1 == '']           <- NA
Ethanol.prod$Marketing.year.and.quarter1           <- as.numeric(substr(Ethanol.prod$Marketing.year.and.quarter1, 1, 4))

Ethanol.prod$Marketing.year.and.quarter1           <- imputeTS::na.locf(Ethanol.prod$Marketing.year.and.quarter1)

Ethanol.prod                                       <- Ethanol.prod[grep('MY Sep-Aug', Ethanol.prod$X), ]
Ethanol.prod                                       <- Ethanol.prod %>% dplyr::select(c(1, 4, 6, 7, 9))
Ethanol.prod                                       <- Ethanol.prod %>% filter(Marketing.year.and.quarter1 > 1989)
colnames(Ethanol.prod)[1]                          <- 'Year'

### Ethanol.infrastructure

colnames(Ethanol.infrastructure)[1]                <- 'Year'
Ethanol.infrastructure                             <- Ethanol.infrastructure %>% group_by(Year) %>% 
                                                          summarise(Processing.Capacity = mean(Processing.Capacity, na.rm = TRUE))


Processing <- c(rep(Ethanol.infrastructure$Processing.Capacity[1], times = 10), Ethanol.infrastructure$Processing.Capacity)

##########################################################################

### Merge

##########################################################################

Corn  <- merge(Corn, Ethanol.econ, all.x = TRUE)
Corn  <- merge(Corn, Ethanol.prod, all.x = TRUE)
Corn  <- merge(Corn, Biofuel.subsidies[, c(1, 4, 8, 9)])
Corn$Processing.Cap <- Processing


setwd('E:/Modules/Dissertation/Data/Model Ready/Price')
#write.csv(Corn, 'Corn_price.csv', row.names = FALSE)



#######################################################################################################################

### Misc data tidying

#######################################################################################################################

###################################################################################

### use Seed amount for extense not Seed cost

###################################################################################


setwd('E:/Modules/Dissertation/Data/Intensification/Fertiliser by Commod_Processed')

GMO.commod <- read.csv('GMO_commod.csv')

setwd('E:/Modules/Dissertation/Data/Model Ready')

Corn.subs <- read.csv('Corn_subsidies.csv')
Soy.subs  <- read.csv('Soy_subsidies.csv')

Corn.all  <- data.frame(Year = 1990:2017, OverallANSI = rep(unique(Corn.subs$OverallANSI), each = 28))
Corn.all  <- merge(Corn.all, Corn.subs, all.x = TRUE)
Corn.all  <- data.frame(Corn.all %>% filter(!is.na(OverallANSI)))
Corn.all  <- data.frame(Corn.all %>% dplyr::select(c(1, 2, 32, 33)))
Corn.all  <- split(Corn.all, Corn.all$OverallANSI)
Corn.GM   <- lapply(Corn.all, function(x) {imputeTS::na.interpolation(x[, c(3, 4)])})

Corn.state<- Corn.subs %>% group_by(.dots = c('Year', 'State')) %>% summarise_at(c(32, 33), mean, na.rm = TRUE)


################################################################################

### Make new Soy frame

################################################################################


Soy.derivative <- Soy

Soy.derivative$USprod_Subtract_China       <- Soy$US.Production - Soy$US.China.Exports
Soy.derivative$Chinaimport_Subtract_Brazil <- Soy$Total.China.Imports - Soy$Brazil.China.Exports
Soy.derivative$USprod_Subtract_Biodiesel   <- Soy$US.Production - Soy$US.Biodiesel.Production

Soy.derivative <- data.frame(Soy.derivative %>% dplyr::select(1, 2, 4, 13:15))

fit <- lm(Soy.derivative$Price ~ Soy.derivative$USprod_Subtract_China + Soy.derivative$Chinaimport_Subtract_Brazil + Soy.derivative$USprod_Subtract_Biodiesel)
plot(Soy.derivative$Price, Soy.derivative$USprod_Subtract_Biodiesel)

abline(fit)


###################################################################################################

### Sort out ANSI keys for data

###################################################################################################

setwd('E:/Modules/Dissertation/Data/Model Ready')

Corn.intense <- read.csv('Corn_subsidies.csv')
Soy.intense  <- read.csv('Soy_subsidies.csv')


setwd('E:/Modules/Dissertation/Data/Model Ready/Economic model')

Corn.economic <- read.csv('Corn_economic.csv')
Soy.economic  <- read.csv('Soy_economic.csv')


colnames(Corn.intense) %in% colnames(Corn.economic)

Corn.intense <- Corn.intense[, colnames(Corn.intense) %in% colnames(Corn.median)]

Cornmedian.merge <- merge(Corn.median, Corn.economic[, which(colnames(Corn.economic) %in% 
                                    c('Year', 'USDAagregion', 'Yield', 'Acres_Planted', 'OverallANSI'))], 
                          by = c('Year', 'USDAagregion', 'Yield', 'Acres_Planted'), all.x = TRUE)

Cornmedian.merge <- Cornmedian.merge[Cornmedian.merge$USDAagregion %in% 1:6, ]

Cornmedian.merge <- Cornmedian.merge[!is.na(Cornmedian.merge$OverallANSI), ]

test <- data.frame(Cornmedian.merge$Year, Cornmedian.merge$OverallANSI) 
test <- test %>% dplyr::filter(!is.na(Cornmedian.merge.OverallANSI))


which(duplicated(test))



#######################################################################################################

### Redo intensification data

#######################################################################################################

setwd('E:/Modules/Dissertation/Data/Model Ready')

Corn.intense <- read.csv('Corn_intense_USDA.csv')
Corn.intense <- Corn.intense[Corn.intense$USDAagregion %in% 1:6, ]

Soy.intense  <- read.csv('Soy_intense_USDA.csv')
Soy.intense  <- Soy.intense[Soy.intense$USDAagregion %in% 1:6, ]

Corn.complete<- data.frame(Year = 1990:2017, OverallANSI = rep(unique(Corn.intense$OverallANSI), each = 28))
Soy.complete <- data.frame(Year = 1990:2017, OverallANSI = rep(unique(Soy.intense$OverallANSI), each = 28))

Corn.complete<- merge(Corn.complete, Corn.intense, all.x = TRUE)
Soy.complete <- merge(Soy.complete, Soy.intense, all.x = TRUE)

Corn.complete<- split(Corn.complete, Corn.complete$OverallANSI)
Soy.complete <- split(Soy.complete, Soy.complete$OverallANSI)


for(i in 1:length(Corn.complete)) {
  
  for(j in 1:ncol(Corn.complete[[i]])) {
    
    try(
    
    Corn.complete[[i]][, j] <- imputeTS::na.interpolation(Corn.complete[[i]][, j])
   
    )
     
  }
  
}

Corn.complete <- rbind.fill(Corn.complete)

###

for(i in 1:length(Soy.complete)) {
  
  for(j in 1:ncol(Soy.complete[[i]])) {
    
    try(
      
      Soy.complete[[i]][, j] <- imputeTS::na.interpolation(Soy.complete[[i]][, j])
      
    )
    
  }
  
}

Soy.complete <- rbind.fill(Soy.complete)


####################################################

### Median impute

####################################################

Soy.complete <- Soy.complete[, -37]

Soy.median  <- data.frame(Soy.complete %>% group_by(.dots = c('Year', 'USDAagregion')) %>%
                  summarise_at(11:ncol(Soy.complete), median, na.rm = TRUE))

Soy.median.complete <- Soy.complete

for(i in 11:ncol(Soy.complete)) {
  
  for(j in 1:nrow(Soy.complete)) {
    
    if(is.na(Soy.median.complete[j, i])) {
    
      try(
      
    Soy.median.complete[j, i] <- Soy.median[which(Soy.median$USDAagregion == Soy.median.complete$USDAagregion[j] &
                                                    Soy.median$Year == Soy.median.complete$Year[j]), 
                                            colnames(Soy.median) == colnames(Soy.median.complete)[i]]
    
      )
    
    }
      
  }
  
  print(i)
  
}

Soy.median.complete     <- data.frame(Soy.median.complete %>% filter(!is.na(USDAagregion)))

Soy.median.complete$irrigatedpct <- Soy.median.complete$SOYBEANS..IRRIGATED...ACRES.PLANTED / Soy.median.complete$Acres_Planted
Soy.median.complete$irrigatedpct <- ifelse(Soy.median.complete$irrigatedpct > 1, 1, Soy.median.complete$irrigatedpct)
Soy.median.complete$irrigatedpct <- ifelse(is.na(Soy.median.complete$irrigatedpct), 0, Soy.median.complete$irrigatedpct)

Soy.median.filter     <- data.frame(Soy.median.complete %>% select(1,2, 10:11, 14,15, 19, 26, 28, 29, 30, 34:37, 40:43))


### Corn

Corn.median  <- data.frame(Corn.complete %>% group_by(.dots = c('Year', 'USDAagregion')) %>%
                            summarise_at(11:ncol(Soy.complete), median, na.rm = TRUE))

Corn.median.complete <- Corn.complete

for(i in 11:ncol(Corn.complete)) {
  
  for(j in 1:nrow(Corn.complete)) {
    
    if(is.na(Corn.median.complete[j, i])) {
      
      try(
        
        Corn.median.complete[j, i] <- Corn.median[which(Corn.median$USDAagregion == Corn.median.complete$USDAagregion[j] &
                                                          Corn.median$Year == Corn.median.complete$Year[j]), 
                                                which(colnames(Corn.median) == colnames(Corn.median.complete)[i])]
        
      )
      
    }
    
  }
  
  print(i)
  
}

Corn.median.complete     <- data.frame(Corn.median.complete %>% filter(!is.na(USDAagregion)))

Corn.median.complete$irrigatedpct <- Corn.median.complete$CORN..IRRIGATED...ACRES.PLANTED / Corn.median.complete$Acres_Planted
Corn.median.complete$irrigatedpct <- ifelse(Corn.median.complete$irrigatedpct > 1, 1, Corn.median.complete$irrigatedpct)
Corn.median.complete$irrigatedpct <- ifelse(is.na(Corn.median.complete$irrigatedpct), 0, Corn.median.complete$irrigatedpct)

Corn.median.filter     <- data.frame(Corn.median.complete %>% select(1,2, 10:11, 14,15, 19, 24,25, 29, 30:38, 42:45))

setwd('E:/Modules/Dissertation/Data/FinalCsvs/Intense')

#write.csv(Corn.median.filter, 'Corn.median.csv', row.names = FALSE)
#write.csv(Soy.median.filter, 'Soy.median.csv', row.names = FALSE)
#write.csv(Corn.complete, 'Corn.NA.csv', row.names = FALSE)
#write.csv(Soy.complete, 'Soy.NA.csv', row.names = FALSE)

##################################################################################################################

###################################################################################################################

setwd('E:/Modules/Dissertation/Data/FinalCsvs/Intense')

Corn.median <- read.csv('Corn.median.csv')
Soy.median  <- read.csv('Soy.median.csv')

for(i in 1:nrow(Corn.median)) {
  
  if(Corn.median$irrigatedpct[i] == 0 & Corn.median$Year >= 1997) {
    
    Corn.median$irrigatedpct[i] <- median(Corn.median$irrigatedpct[Corn.median$Year == Corn.median$Year[i] &
                                                                     Corn.median$USDAagregion == Corn.median$USDAagregion[i]])
    
  }
     
  
}

for(i in 1:nrow(Soy.median)) {
  
  if(Soy.median$irrigatedpct[i] == 0 & Soy.median$Year >= 1997) {
    
    Soy.median$irrigatedpct[i] <- median(Soy.median$irrigatedpct[Soy.median$Year == Soy.median$Year[i] &
                                                                   Soy.median$USDAagregion == Soy.median$USDAagregion[i]])
    
  }
  
  print(i)
}




#write.csv(Soy.median.filter, 'Soy.median.csv', row.names = FALSE)
#write.csv(Corn.complete, 'Corn.NA.csv', row.names = FALSE)
#write.csv(Soy.complete, 'Soy.NA.csv', row.names = FALSE)


#######################################################################

setwd('E:/Modules/Dissertation/Data/FinalCsvs/Intense')

Corn.median <- read.csv('Corn.median.csv')
Soy.median  <- read.csv('Soy.median.csv')

unique(table(Corn.median$OverallANSI))
unique(table(Soy.median$OverallANSI))

key         <- as.numeric(names(table(Corn.median$OverallANSI)[table(Corn.median$OverallANSI) == 1]))
Corn.median <- Corn.median[!Corn.median$OverallANSI %in% key, ]

key         <- as.numeric(names(table(Soy.median$OverallANSI)[table(Soy.median$OverallANSI) == 1]))
Soy.median  <- Soy.median[!Soy.median$OverallANSI %in% key, ]

write.csv(Corn.median, 'Corn.median.csv', row.names = FALSE)
write.csv(Soy.median, 'Soy.median.csv', row.names = FALSE)


###########################################################################

### Make economic all years

###########################################################################

### 4) Profit Loss model

setwd('E:/Modules/Dissertation/Data/FinalCsvs/Economic')

Corn.Economic <- read.csv('Corn_economic.csv')
Soy.Economic  <- read.csv('Soy_economic.csv')

Corn.Economic.Complete <- data.frame(Year = 1990:2017, OverallANSI = rep(unique(Corn.Economic$OverallANSI), each  = 28))
Soy.Economic.Complete  <- data.frame(Year = 1990:2017, OverallANSI = rep(unique(Soy.Economic$OverallANSI), each  = 28))

Corn.Economic.Complete <- merge(Corn.Economic.Complete, Corn.Economic, by = c('Year', 'OverallANSI'),
                                all.x = TRUE)

Soy.Economic.Complete <- merge(Soy.Economic.Complete, Soy.Economic, by = c('Year', 'OverallANSI'),
                                all.x = TRUE)

Corn.Economic.Complete <- split(Corn.Economic.Complete, Corn.Economic.Complete$OverallANSI)
Soy.Economic.Complete  <- split(Soy.Economic, Soy.Economic$OverallANSI)

for(i in 1:length(Soy.Economic.Complete)) {

  for(j in 4:ncol(Soy.Economic.Complete[[i]])) {
  
    try(
    
      Soy.Economic.Complete[[i]][, j] <- imputeTS::na.interpolation(Soy.Economic.Complete[[i]][, j])
  
    )
    
    #Soy.Economic.Complete[[i]][, j] <- imputeTS::na.interpolation(Soy.Economic.Complete[[i]][, j])

    
    }

}
  
Corn.Economic <- rbind.fill(Corn.Economic.Complete)
Corn.Economic <- Corn.Economic[complete.cases(Corn.Economic), ]

key           <- names(table(Corn.Economic$OverallANSI)[table(Corn.Economic$OverallANSI) == 1])
Corn.Economic <- Corn.Economic[!Corn.Economic$OverallANSI %in% key,]

Soy.Economic <- rbind.fill(Soy.Economic.Complete)
Soy.Economic <- Soy.Economic[complete.cases(Soy.Economic[, -c(3)]), ]

key.Soy      <- names(table(Soy.Economic$OverallANSI)[table(Soy.Economic$OverallANSI) == 1])
Soy.Economic <- Soy.Economic[!Soy.Economic$OverallANSI %in% key.Soy,]

Corn.Economic <- split(Corn.Economic, Corn.Economic$USDAagregion)
Soy.Economic  <- split(Soy.Economic, Soy.Economic$USDAagregion)
Soy.Economic  <- Soy.Economic[1:6]

setwd('E:/Modules/Dissertation/Data/FinalCsvs/Economic')

write.csv(rbind.fill(Corn.Economic), 'Corn_economic2.csv', row.names = FALSE)
write.csv(rbind.fill(Soy.Economic), 'Soy_economic2.csv', row.names = FALSE)


###################################################################################

### Sort Soy Wastage

###################################################################################

setwd('E:/Modules/Dissertation/Data/Model Ready')

Corn.subs <- read.csv('Corn_subsidies.csv')
Soy.subs  <- read.csv('Soy_subsidies.csv')

setwd('E:/Modules/Dissertation/Data/FinalCsvs/Economic')

Corn <- read.csv('Corn_economic2.csv')
Soy  <- read.csv('Soy_economic2.csv')

Soy.subs$Wastage <- Soy.subs$SOYBEANS...ACRES.HARVESTED / Soy.subs$Acres_Planted

Soy.subs.Complete<- data.frame(Year = 1990:2017, OverallANSI = rep(unique(Soy.subs$OverallANSI), each = 28))
                                 
Soy.subs.Complete<- merge(Soy.subs.Complete, Soy.subs[, c(1, 2, 42)], all.x = TRUE)

Soy.subs.Complete<- split(Soy.subs.Complete, Soy.subs.Complete$OverallANSI)

for(i in 1:length(Soy.subs.Complete)) {
  
  try(
    
    
    Soy.subs.Complete[[i]]$Wastage <- imputeTS::na.interpolation(Soy.subs.Complete[[i]]$Wastage) 
    
  )
  
}

Soy.subs.Complete <- rbind.fill(Soy.subs.Complete)

Soy <- Soy[, -c(which(colnames(Soy) == 'Wastage'))]

Soy <- merge(Soy, Soy.subs.Complete, all.x = TRUE, by = c('Year', 'OverallANSI'))

write.csv(Soy, 'Soy_economic2.csv', row.names= FALSE)

#############################################################################################



#############################################################################################

### Update CRP

#############################################################################################

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Agricultural context')

CRP.enrol <- read.csv('CRP payment amount.csv', skip = 2)
CRP.pay   <- read.csv('CRP Rental Payment History By County1.csv', skip = 2)

CRP.enrol <- CRP.enrol[-1, ]
CRP.pay   <- CRP.pay[-1, ]

CRP.enrol <- CRP.enrol[!is.na(CRP.enrol$STATE) & !is.na(CRP.enrol$FIPS), ]
CRP.pay   <- CRP.pay[!is.na(CRP.pay$STATE) & !is.na(CRP.pay$FIPS), ]

colnames(CRP.enrol) <- gsub('X', '', colnames(CRP.enrol))
colnames(CRP.pay) <- gsub('X', '', colnames(CRP.pay))

CRP.enrol[, 4:ncol(CRP.enrol)] <- apply(CRP.enrol[, 4:ncol(CRP.enrol)], 2, function(x) {as.numeric(gsub(',', '', x))})
CRP.pay[, 4:ncol(CRP.pay)] <- apply(CRP.pay[, 4:ncol(CRP.pay)], 2, function(x) {as.numeric(gsub(',', '', x))})

CRP.pay <- CRP.pay[, -c(5:8)]
CRP.enrol <- CRP.enrol[, -c(4:6, 35)]

CRP.enrol.long <- tidyr::gather(CRP.enrol[, 4:ncol(CRP.enrol)])
CRP.enrol.long <- CRP.enrol.long[as.numeric(CRP.enrol.long$key) > 1989 & as.numeric(CRP.enrol.long$key) < 2018, ]

CRP.pay.long <- tidyr::gather(CRP.pay[, 4:ncol(CRP.pay)])
CRP.pay.long <- CRP.pay.long[CRP.pay.long$key > 1989 & CRP.pay.long$key < 2018, ]

CRP.pay.long$OverallANSI   <- rep(CRP.pay$FIPS, times = 27)
CRP.enrol.long$OverallANSI <- rep(CRP.enrol$FIPS, times = 28)

CRP.pay.long <- split(CRP.pay.long, CRP.pay.long$OverallANSI)

for(i in 1:length(CRP.pay.long)) {
  
  CRP.pay.long[[i]][28, ] <- data.frame(Year = 1990, value = NA, OverallANSI = CRP.pay.long[[i]][1, 3])
  
  CRP.pay.long[[i]]       <- CRP.pay.long[[i]][order(CRP.pay.long[[i]]$key), ]
  
}


for(i in 1:length(CRP.pay.long)) {

  try(
  
CRP.pay.long[[i]] <- imputeTS::na.interpolation(CRP.pay.long[[i]])

)

}

CRP.pay.long   <- rbind.fill(CRP.pay.long)
CRP.enrol.long <- CRP.enrol.long[order(CRP.enrol.long$OverallANSI), ]
  
colnames(CRP.pay.long) <- c('Year', 'CRP.Payment', 'OverallANSI')
colnames(CRP.enrol.long) <- c('Year', 'CRP.Enrollment', 'OverallANSI')

test <- CRP.enrol.long$CRP.Enrollment / CRP.pay.long$CRP.Payment

test[is.infinite(test)] <- median(test, na.rm = TRUE) 

CRP.pay.long$Per_Acre_payment <- test

for(i in 1:nrow(CRP.pay.long)) {
  
  if(is.na(CRP.pay.long$Per_Acre_payment[i]) & CRP.pay.long$CRP.Payment[i] == 0) {
  
  CRP.pay.long$Per_Acre_payment[i] <- 0
    
  }
  
  print(i)
  
}

CRP.pay.long$Per_Acre_payment[CRP.pay.long$Per_Acre_payment > quantile(CRP.pay.long$Per_Acre_payment, 0.99)] <- quantile(CRP.pay.long$Per_Acre_payment, 0.99)


Expectation.test <- merge(Expectation.Corn, CRP.pay.long, all.x = TRUE)



##########################################################################################

### Correct State NA screw up in Extensification Data

##########################################################################################

setwd('E:/Modules/Dissertation/Data/FinalCsvs/Economic')

Soy <- read.csv('Soy_economic2.csv', stringsAsFactors = FALSE)
Corn<- read.csv('Corn_economic2.csv')

get_mode <- function(x) {
  
  uniqx <- unique(x)
  uniqx[which.max(table(uniqx))]
  
}

### Log state vals, then correct state

Modes.df   <- Soy %>% group_by(.dots = c('USDAagregion', 'Year', 'State')) %>% summarise_all(get_mode)

Soy$State2 <- Soy$State

Soy <- split(Soy, Soy$USDAagregion)


for(i in 1:length(Soy)) {

      Soy[[i]]$State <- zoo::na.locf(Soy[[i]]$State)
      
    }
    


for(i in 1:length(Soy)) {
  
  for(j in 1:nrow(Soy[[i]])) {
    
    if(is.na(Soy[[i]]$State2[j])) {
      
      Soy[[i]][j, c(8:16, 18:19)] <- Modes.df[which(Soy[[i]]$Year[j] == Modes.df$Year &
                                                   Soy[[i]]$State[j] == Modes.df$State &
                                                   Soy[[i]]$USDAagregion[j] == Modes.df$USDAagregion), 
                                              c(8:16, 18:19)]
      
    }
    
  }
  
  print(i)
  
}


Soy <- rbind.fill(Soy)

Soy$Total.subsidy  <- Soy$Generic_subsidy + Soy$ACRO_CO.Subsidy + Soy$Insurance

Soy$Costs          <- Soy$Seed + Soy$Chemicals + Soy$Fertiliser +
                         Soy$Repairs + Soy$Customservices + Soy$Overhead

Soy$Margin         <- (Soy$Yield * Soy$SOYBEANS...PRICE.RECEIVED..MEASURED.IN.....BU *
                         Soy$Wastage) - Soy$Costs + Soy$Total.subsidy

Soy <- Soy[, -22]

#write.csv(Soy, 'Soy_economicv3.csv', row.names = FALSE)
#write.csv(Corn, 'Corn_economicv3.csv', row.names = FALSE)


####################################################################################

### Lag price for Extensive and Intensive

####################################################################################

Corn.Price <- read.csv('E:/Modules/Dissertation/Data/Model Ready/Price/Corn_price.csv')
Soy.Price  <- read.csv('E:/Modules/Dissertation/Data/Model Ready/Price/Soy_price.csv')

setwd('E:/Modules/Dissertation/Data/FinalCsvs/Extense')

Soy <- read.csv('Soy_Extense.csv')
Corn<- read.csv('Corn_Extense.csv')

test <- Soy %>% group_by(Year) %>% summarise(Price = mean(Price))
test$Year <- 1991:2018 
test <- test[-nrow(test), ]
test[28, ] <- NA
test[28, 1]<- 1990
test[28, 2]<- test[1, 2]

test <- test[order(test$Year), ]

Soy <- merge(Soy, test, by = 'Year', all.x = TRUE)

colnames(Soy)[19:20] <- c('Price', 'Price_lag1')


test <- Corn %>% group_by(Year) %>% summarise(Price = mean(Price))
test$Year <- 1991:2018 
test <- test[-nrow(test), ]
test[28, ] <- NA
test[28, 1]<- 1990
test[28, 2]<- test[1, 2]

test <- test[order(test$Year), ]

Corn <- merge(Corn, test, by = 'Year', all.x = TRUE)

colnames(Corn)[19:20] <- c('Price', 'Price_lag1')

Corn <- split(Corn, Corn$USDAagregion.x)

lapply(Soy, function(x) {cor.test(x$DELTA_Soy[x$DELTA_Soy > -10000 & x$DELTA_Soy < 10000], 
                                  x$Price_lag1[x$DELTA_Soy > -10000 & x$DELTA_Soy < 10000], method = 'kendall')})

########

### Add lag1 Delta (account for rotation)

########

Corn <- rbind.fill(Corn)
Soy  <- rbind.fill(Soy)

Soy$DELTA_Soy_LAG1   <- Soy$DELTA_Soy
Corn$DELTA_Corn_LAG1 <- Corn$DELTA_Corn

Soy <- split(Soy, Soy$OverallANSI)
Corn<- split(Corn, Corn$OverallANSI)


for(i in 1:length(Soy)) {

  Soy[[i]]$DELTA_Soy_LAG1 <- c(Soy[[i]]$DELTA_Soy_LAG1[1], Soy[[i]]$DELTA_Soy_LAG1[1:27])

  Corn[[i]]$DELTA_Corn_LAG1 <- c(Corn[[i]]$DELTA_Corn_LAG1[1], 
                                 Corn[[i]]$DELTA_Corn_LAG1[1:27])
  
}

Soy <- rbind.fill(Soy)
Corn<- rbind.fill(Corn)

Soy <- split(Soy, Soy$USDAagregion.x)
Corn<- split(Corn, Corn$USDAagregion.x)

lapply(Soy, function(x) {summary(lm(x$DELTA_Soy ~ x$DELTA_Soy_LAG1 + x$Price_lag1))})


###############################################################

### Northern Extensive Margin

###############################################################

Pre_GM_Planted       <- Soy[Soy$Year == 1996, ] %>% dplyr::select(c(3, 4))

PRE_GM       <- apply(Soy,1, function(x) {
  Pre_GM_Planted[which(Pre_GM_Planted$OverallANSI == x[3]), ]$SOY_Acres_Planted.y 
})

PRE_GM <- numeric()

for(i in 1:nrow(Soy)) {
  
PRE_GM[i] <- Pre_GM_Planted[which(Pre_GM_Planted$OverallANSI == Soy$OverallANSI[i]), ]$SOY_Acres_Planted.y
  
}

Pre_GM_Percent <- numeric()

for(i in 1:nrow(Soy)) {
  
  Pre_GM_Percent[i] <- Soy$SOY_Acres_Planted.y[i] / Pre_GM_Planted[which(Pre_GM_Planted$OverallANSI == Soy$OverallANSI[i]), ]$SOY_Acres_Planted.y
  
}

Soy$PreGM_Acres          <- bnlearn::discretize(data.frame(PRE_GM))[, 1]
Soy$Pre_GM.pct           <- Pre_GM_Percent

Soy <- split(Soy, Soy$USDAagregion.x)
Soy <- rbind.fill(Soy)


### Corn

Pre_GM_Planted       <- Corn[Corn$Year == 1996, ] %>% dplyr::select(c(3, 4))

PRE_GM <- numeric()

for(i in 1:nrow(Corn)) {
  
  PRE_GM[i] <- Pre_GM_Planted[which(Pre_GM_Planted$OverallANSI == Corn$OverallANSI[i]), ]$CORN_Acres_Planted.x
  
}

Pre_GM_Percent <- numeric()

for(i in 1:nrow(Soy)) {
  
  Pre_GM_Percent[i] <- Corn$CORN_Acres_Planted.x[i] / Pre_GM_Planted[which(Pre_GM_Planted$OverallANSI == Corn$OverallANSI[i]), ]$CORN_Acres_Planted.x
  
}

Corn$PreGM_Acres          <- bnlearn::discretize(data.frame(PRE_GM))[, 1]
Corn$Pre_GM.pct           <- Pre_GM_Percent
Corn                      <- split(Corn, Corn$USDAagregion.x)


Corn<- rbind.fill(Corn)
Soy <- rbind.fill(Soy)


##################################################################################

### PLOTS

##################################################################################

lapply(Soy, function(x) {summary(lm(x$DELTA_Soy ~ x$Pre_GM.pct + x$PreGM_Acres + x$Price_lag1 + x$DELTA_Soy_LAG1))})

dummy <- lapply(Soy, function(x) {x %>% group_by(.dots = c('Year', 'PreGM_Acres')) %>% 
    summarise(SOY_Acres_Planted.y = mean(SOY_Acres_Planted.y), DELTA = mean(DELTA_Soy))})

ggplot(dummy[[3]], aes(x = Year, y = SOY_Acres_Planted.y, colour = PreGM_Acres)) + geom_line()

## This trend has to be captured well


##################################################################################

### Sort out margins & CRP payments etc.

##################################################################################


setwd('E:/Modules/Dissertation/Data/FinalCsvs/Extense')

Soy <- read.csv('Soy_ExtenseV3.csv')
Corn<- read.csv('Corn_ExtenseV3.csv')

setwd('E:/Modules/Dissertation/Data/FinalCsvs/Economic')

Soy.Economic <- read.csv('Soy_economicv3.csv')
Corn.Economic<- read.csv('Corn_economicv3.csv')


##################################################################################

### Compare Expectation

##################################################################################

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
    
    slideFunct(x$Yield * x$SOYBEANS...PRICE.RECEIVED..MEASURED.IN.....BU * x$Wastage, window = 3, step = 1)
    
  })
  
  temp <- lapply(temp, function(x) {
    
    c(rep(x[1], times =3),x)
    
  })
  
  temp           <- unlist(temp)
  
  data$Expection.simple <- temp
  
  return(data)
  
}

Soy.Economic <- split(Soy.Economic, Soy.Economic$OverallANSI) 
Soy.Economic <- lapply(Soy.Economic, create.expect)
Soy.Economic <- rbind.fill(Soy.Economic)
Soy.Economic$Expection <- Soy.Economic$Expection + Soy.Economic$Total.subsidy - Soy.Economic$Costs


Corn.Economic<- split(Corn.Economic, Corn.Economic$OverallANSI)
Corn.Economic <- lapply(Corn.Economic, create.expect)
Corn.Economic<- rbind.fill(Corn.Economic)
Corn.Economic$Expection <- Corn.Economic$Expection + Corn.Economic$Total.subsidy - Corn.Economic$Costs


Corn.Economic2 <- merge(Corn.Economic, CRP.pay.long[, c(1, 3, 4)], by= c('Year', 'OverallANSI'), all.x = TRUE)
Soy.Economic2  <- merge(Soy.Economic, CRP.pay.long[, c(1, 3, 4)], by= c('Year', 'OverallANSI'), all.x = TRUE)


#*******************************************************************************************

#######################################################################################
setwd('E:/Modules/Dissertation/Data/FinalCsvs/Economic')
#######################################################################################

#*******************************************************************************************


Corn.Economic <- read.csv('Corn_economicv4.csv')
Soy.Economic  <- read.csv('Soy_economicv4.csv')

colnames(Corn.Economic)[25] <- 'CRP_PerAcre_Payment'
colnames(Soy.Economic)[24]  <- 'CRP_PerAcre_Payment'

setwd('E:/Modules/Dissertation/Data/FinalCsvs/Extense')

Soy <- read.csv('Soy_ExtenseV3.csv')
Corn<- read.csv('Corn_ExtenseV3.csv')

Soy <- Soy[Soy$USDAagregion.x %in% 1:6, ]
Corn<- Corn[Corn$USDAagregion.x %in% 1:6, ]


############################################################

### Calculate Expection Differences

############################################################

Soy.Economic$SOYCRPDifference <- Soy.Economic$Expection - Soy.Economic$CRP_PerAcre_Payment
Corn.Economic$CORNCRPDifference <- Corn.Economic$Expection - Corn.Economic$CRP_PerAcre_Payment

Soy.Economic$SOYCRPDifference.Simple    <-  Soy.Economic$Expection.simple - Soy.Economic$CRP_PerAcre_Payment
Corn.Economic$CORNCRPDifference.Simple  <- Corn.Economic$Expection.simple - Corn.Economic$CRP_PerAcre_Payment

Corn$EXPECTED.SoyCorn_difference <- NA
Soy$EXPECTED.SoyCorn_difference <- NA

for(i in 1:nrow(Corn)) {
  
  try(
  
  Corn$EXPECTED.SoyCorn_difference[i] <- Soy.Economic$Expection[which(Soy.Economic$Year == Corn$Year[i] & 
                                            Soy.Economic$OverallANSI == Corn$OverallANSI[i])] - Corn.Economic$Expection[which(Corn.Economic$Year == Corn$Year[i] & 
                                                 Corn.Economic$OverallANSI == Corn$OverallANSI[i])]
  )
  
  try(
  
  Soy$EXPECTED.SoyCorn_difference[i] <- Soy.Economic$Expection[which(Soy.Economic$Year == Soy$Year[i] & 
                                            Soy.Economic$OverallANSI == Soy$OverallANSI[i])] - Corn.Economic$Expection[which(Corn.Economic$Year == Soy$Year[i] & 
                                                Corn.Economic$OverallANSI == Soy$OverallANSI[i])]
  
  )
  

}

Corn.median <- Corn.Economic %>% group_by(.dots = c('Year', 'USDAagregion')) %>% summarise_at(5:ncol(Corn.Economic), median, na.rm = TRUE)
Soy.median  <- Soy.Economic %>% group_by(.dots = c('Year', 'USDAagregion')) %>% summarise_at(5:ncol(Soy.Economic), median, na.rm = TRUE)



for(i in 1:nrow(Corn)) {
  
  if(is.na(Corn$EXPECTED.SoyCorn_difference[i])) {
    
    Corn$EXPECTED.SoyCorn_difference[i] <- Soy.median$Expection[which(Soy.median$Year == Corn$Year[i] & 
                                                                        Soy.median$USDAagregion == Corn$USDAagregion.x[i])] - Corn.median$Expection[which(Corn.median$Year == Corn$Year[i] & 
                                                                                                                                                            Corn.median$USDAagregion == Corn$USDAagregion.x[i])]
  }
  
  if(is.na(Soy$EXPECTED.SoyCorn_difference[i])) {
    
    Soy$EXPECTED.SoyCorn_difference[i] <- Soy.median$Expection[which(Soy.median$Year == Soy$Year[i] & 
                                                                       Soy.median$USDAagregion == Soy$USDAagregion.x[i])] - Corn.median$Expection[which(Corn.median$Year == Soy$Year[i] & 
                                                                                                                                                          Corn.median$USDAagregion == Soy$USDAagregion.x[i])]
    
  }
  
  
}


###############################
### Do CRP
###############################

Corn$EXPECTED.CORNCRP_difference <- NA
Soy$EXPECTED.SOYCRP_difference   <- NA


for(i in 1:nrow(Corn)) {
  
  
  try(
    
    Corn$EXPECTED.CORNCRP_difference[i] <- Corn.Economic$CORNCRPDifference[which(Corn.Economic$Year == Corn$Year[i] & 
                                                    Corn.Economic$OverallANSI == Corn$OverallANSI[i])] 
  )
  
  try(
    
    Soy$EXPECTED.SOYCRP_difference[i]   <- Soy.Economic$SOYCRPDifference[which(Soy.Economic$Year == Soy$Year[i] & 
                                                    Soy.Economic$OverallANSI == Soy$OverallANSI[i])] 
    
  )
  
  
  if(is.na(Corn$EXPECTED.CORNCRP_difference[i])) {
    
    Corn$EXPECTED.CORNCRP_difference[i] <- Corn.median$CORNCRPDifference[which(Corn.median$Year == Corn$Year[i] & 
                                                     Corn.median$USDAagregion == Corn$USDAagregion.x[i])] 
  }
  
  if(is.na(Soy$EXPECTED.SoyCorn_difference[i])) {
    
    Soy$EXPECTED.SOYCRP_difference[i]   <- Soy.median$SOYCRPDifference[which(Soy.median$Year == Soy$Year[i] & 
                                                      Soy.median$USDAagregion == Soy$USDAagregion.x[i])] 
    
  }
  
  
}

### drop alternate CRP

Corn <- Corn[, -7]
Soy  <- Soy[, -8]


setwd('E:/Modules/Dissertation/Data/FinalCsvs/Extense')

#write.csv(Soy, 'Soy_ExtenseV4.csv', row.names = FALSE)
#write.csv(Corn, 'Corn_ExtenseV4.csv', row.names = FALSE)


###########################################################################
##################################################################

## Yield predictive accuracy

##################################################################
############################################################################


setwd('E:/Modules/Dissertation/Data/FinalCsvs/Intense')

Corn <- read.csv('Corn.median2.csv')
Soy  <- read.csv('Soy.median2.csv')

setwd('E:/Modules/Dissertation/Data/FinalCsvs/Economic')

Corn.Economic <- read.csv('Corn_economicv4.csv')
Soy.Economic  <- read.csv('Soy_economicv4.csv')

colnames(Corn.Economic)[25] <- 'CRP_PerAcre_Payment'
colnames(Soy.Economic)[24]  <- 'CRP_PerAcre_Payment'

for(i in 1:nrow(Corn)) {
  
  try(
    
    Corn$Expection[i] <- Corn.Economic$Expection[which(Corn.Economic$Year == Corn$Year[i] & 
                            Corn.Economic$OverallANSI == Corn$OverallANSI[i])]
  )
  
  print(i)
  
  
  
}

for(i in 1:nrow(Soy)) {

try(
  
  Soy$Expection[i] <- Soy.Economic$Expection[which(Soy.Economic$Year == Soy$Year[i] & 
                            Soy.Economic$OverallANSI == Soy$OverallANSI[i])]
  
)
  
  print(i)
  
}


Soy$GM[Soy$Year < 1997] <- 0
Corn$GM_Herbicide[Corn$Year < 1997] <- 0
Corn$GM_Insecticide[Corn$Year < 1997] <- 0
Corn$GM_Herbicide[Corn$Year < 1997] <- 0
Corn$GM_Herbicide[Corn$Year < 1997] <- 0
colnames(Corn)[6] <- 'Half.Planted.Day'

### Make price lag1

colnames(Corn)[5] <- 'Price_lag1'
colnames(Soy)[5] <- 'Price_lag1'

Corn <- split(Corn, Corn$OverallANSI)
Soy  <- split(Soy, Soy$OverallANSI)



for(i in 1:length(Soy)) {
  
  Soy[[i]]$Price_lag1 <- c(Soy[[i]]$Price_lag1[1], Soy[[i]]$Price_lag1[1:27])
  
  Corn[[i]]$Price_lag1 <- c(Corn[[i]]$Price_lag1[1], 
                                 Corn[[i]]$Price_lag1[1:27])
  
}


for(i in 1771:2027) {
  
  Corn[[i]]$Price_lag1 <- c(Corn[[i]]$Price_lag1[1], 
                            Corn[[i]]$Price_lag1[1:27])
  
}

Corn <- rbind.fill(Corn)
Soy  <- rbind.fill(Soy)


setwd('E:/Modules/Dissertation/Data/FinalCsvs/Intense')

#write.csv(Corn, 'Corn.medianv3.csv', row.names = FALSE)
#write.csv(Soy, 'Soy.medianv3.csv', row.names = FALSE)





#######################################################################################

## Yield a non-linear relationship?

#######################################################################################

Yield.year <- Corn %>% group_by(.dots = c('Year', 'USDAagregion')) %>% summarise(Yield = mean(Yield))

Yield.year$linear <- predict.lm(lm(Yield ~ Year + USDAagregion, data = Yield.year))
Yield.year$log    <- predict.lm(lm(log(Yield) ~ Year + USDAagregion, data = Yield.year))

ggplot(Yield.year[Yield.year$USDAagregion == 1, ], aes(x = Year, y = Yield)) + geom_line() + geom_line(aes(y = linear)) + 
  geom_line(aes(y = exp(log), colour = 'red'))


Yield.year <- Soy %>% group_by(.dots = c('Year', 'USDAagregion')) %>% summarise(Yield = mean(Yield))

Yield.year$linear <- predict.lm(lm(Yield ~ Year + USDAagregion, data = Yield.year))
Yield.year$log    <- predict.lm(lm(log(Yield) ~ Year + USDAagregion, data = Yield.year))

ggplot(Yield.year[Yield.year$USDAagregion == 5, ], aes(x = Year, y = Yield)) + geom_line() + geom_line(aes(y = linear)) + 
  geom_line(aes(y = exp(log), colour = 'red'))


library(Metrics)

rmse(Yield.year$Yield, exp(Yield.year$log))
rmse(Yield.year$Yield, Yield.year$linear)


for(i in 1:6) {

  print(rmse(predict(lm(Yield ~., data = Corn[Corn$USDAagregion == i, -c(1:2)])) 
                     , Corn$Yield[Corn$USDAagregion ==i]))

  print(rmse(exp(predict(lm(log(Yield) ~., data = Corn[Corn$USDAagregion == i, -c(1:2)]))) 
             , Corn$Yield[Corn$USDAagregion ==i]))
  
}

################################################################

### REDO TEMP & PREC

################################################################

setwd('E:/Modules/Dissertation/Data/FinalCsvs/Intense')

Corn.median <- read.csv('Corn.medianv3.csv')
#Corn.median <- Corn.median[, -c(20:22)]
Corn.median <- split(Corn.median, Corn.median$USDAagregion)

Soy.median  <- read.csv('Soy.medianv3.csv')
#Soy.median  <- Soy.median[, -c(16:18)]
Soy.median  <- split(Soy.median, Soy.median$USDAagregion)

#Soy.median  <- merge(Soy.median, Corn.median[, c(1:2, 18)], by = c('Year', 'OverallANSI'), all.x = TRUE)

setwd('E:/Modules/Dissertation/Data/Climate/Final')

temp <- read.csv('tempave_growingseason.csv')
prec <- read.csv('precipitation_growingseason.csv')

Soy.median <- merge(Soy.median, prec[, c(2:4)], by = c('Year', 'OverallANSI'), all.x = TRUE)
Soy.median <- merge(Soy.median, temp[, c(2:4)], by = c('Year', 'OverallANSI'), all.x = TRUE)
colnames(Soy.median)[c(18:19)] <- c('Prec', 'Temp')

lapply(Soy.median, function(x) {print(summary(lm(x$Yield ~ x$Prec * x$Temp)))})

Soy.median <- rbind.fill(Soy.median)[, -c(15)]


Corn.median <- merge(Corn.median, prec[, c(2:4)], by = c('Year', 'OverallANSI'), all.x = TRUE)
Corn.median <- merge(Corn.median, temp[, c(2:4)], by = c('Year', 'OverallANSI'), all.x = TRUE)
colnames(Corn.median)[c(22:23)] <- c('Prec', 'Temp')

lapply(Corn.median, function(x) {print(summary(lm(x$Yield ~ x$Prec + x$Temp)))})

Corn.median <- rbind.fill(Corn.median)

Corn.median <- Corn.median[, -c(18, 19)]

setwd('E:/Modules/Dissertation/Data/FinalCsvs/Intense')

#write.csv(Corn.median, 'Corn.medianv4.csv', row.names = FALSE)
#write.csv(Soy.median, 'Soy.medianv4.csv', row.names = FALSE)

################################################################################################


setwd('E:/Modules/Dissertation/Data/FinalCsvs/Intense')

Corn.median <- read.csv('Corn.medianv4.csv')
Soy.median  <- read.csv('Soy.medianv4.csv')

setwd('E:/Modules/Dissertation/Data/Processed Dat')

Corn.old <- read.csv('Processed_Corn.csv')
Soy.old  <- read.csv('Processed_Soy.csv')

Corn.old    <- Corn.old %>% filter (!is.na(OverallANSI)) %>% group_by(OverallANSI) %>% summarise(State = unique(State))
Corn.median <- merge(Corn.median, Corn.old, by = 'OverallANSI', all.x = TRUE)
Corn.median <- Corn.median[, c(1:3, 22, 4:21)]

Soy.old     <- Soy.old %>% filter (!is.na(OverallANSI)) %>% group_by(OverallANSI) %>% summarise(State = unique(State))
Soy.median  <- merge(Soy.median, Soy.old, by = 'OverallANSI', all.x = TRUE)
Soy.median <- Soy.median[, c(1:3, 19, 4:18)]

remove(Corn.old, Soy.old)

###############################################################################################

### Merge back in GMO

###############################################################################################

GMO$State <- toupper(GMO$State)

Soy.median         <- merge(Soy.median, GMO[, c(1:2, 7)], by = c('Year', 'State'), all.x = TRUE)
Soy.median$Soy_all <- ifelse(is.na(Soy.median$Soy_all) & Soy.median$Year < 1997, 0, Soy.median$Soy_all)

Soy.Agregion       <- data.frame(Soy.median %>% group_by(.dots = c('Year', 'USDAagregion')) %>% summarise(GM = mean(Soy_all, na.rm = TRUE)))

for(i in 1:nrow(Soy.median)) {
  
  if(is.na(Soy.median$Soy_all[i]) & Soy.median$Year[i] >= 1997) {
  
  Soy.median$Soy_all[i] <- Soy.Agregion[which(Soy.Agregion$Year == Soy.median$Year[i] & Soy.Agregion$USDAagregion == Soy.median$USDAagregion[i]), 3]
  
  }
  
  print(i)
    
}


Corn.median                <- merge(Corn.median, GMO[, c(1:2, 3:6)], by = c('Year', 'State'), all.x = TRUE)
Corn.median$Corn_all       <- ifelse(is.na(Corn.median$Corn_all) & Corn.median$Year < 1997, 0, Corn.median$Corn_all)
Corn.median$Corn_Herbicide <- ifelse(is.na(Corn.median$Corn_Herbicide) & Corn.median$Year < 1997, 0, Corn.median$Corn_Herbicide)
Corn.median$Corn_Insecticide <- ifelse(is.na(Corn.median$Corn_Insecticide) & Corn.median$Year < 1997, 0, Corn.median$Corn_Insecticide)
Corn.median$Corn_stacked     <- ifelse(is.na(Corn.median$Corn_stacked) & Corn.median$Year < 1997, 0, Corn.median$Corn_stacked)

Corn.Agregion       <- data.frame(Corn.median %>% group_by(.dots = c('Year', 'USDAagregion')) %>% summarise_at(c(23:26), mean, na.rm = TRUE))

for(i in 1:nrow(Corn.median)) {
  
  if(is.na(Corn.median$Corn_all[i]) & Corn.median$Year[i] >= 1997) {
    
    Corn.median$Corn_all[i]       <- Corn.Agregion[which(Corn.Agregion$Year == Corn.median$Year[i] & Corn.Agregion$USDAagregion == Corn.median$USDAagregion[i]), 3]
    
    Corn.median$Corn_Herbicide[i] <- Corn.Agregion[which(Corn.Agregion$Year == Corn.median$Year[i] & Corn.Agregion$USDAagregion == Corn.median$USDAagregion[i]), 4]
    
    Corn.median$Corn_Insecticide[i] <- Corn.Agregion[which(Corn.Agregion$Year == Corn.median$Year[i] & Corn.Agregion$USDAagregion == Corn.median$USDAagregion[i]), 5]
    
    Corn.median$Corn_stacked[i] <- Corn.Agregion[which(Corn.Agregion$Year == Corn.median$Year[i] & Corn.Agregion$USDAagregion == Corn.median$USDAagregion[i]), 6]
  }
  
  print(i)
  
}

Corn.median$GM_Herbicide   <- Corn.median$Corn_Herbicide
Corn.median$GM_Insecticide <- Corn.median$Corn_Insecticide
Corn.median$GM_Stacked     <- Corn.median$Corn_stacked
Corn.median$GM_total       <- Corn.median$Corn_all

Corn.median                <- Corn.median[, -c(23:26)]

Soy.median$GM              <- Soy.median$Soy_all
Soy.median                 <- Soy.median[, -20]


#### write out

setwd('E:/Modules/Dissertation/Data/FinalCsvs/Intense')

#write.csv(Corn.median, 'Cornmedianv5.csv', row.names = FALSE)
#write.csv(Soy.median, 'Soymedianv5.csv', row.names = FALSE)


############################################################################################

### GM Extense?

############################################################################################

setwd('E:/Modules/Dissertation/Data/FinalCsvs/Intense')

Corn.intense <- read.csv('Cornmedianv5.csv')
Soy.intense  <- read.csv('Soymedianv5.csv')

setwd('E:/Modules/Dissertation/Data/FinalCsvs/Extense')

Corn.extense <- read.csv('Corn_ExtenseV4.csv')
Soy.extense  <- read.csv('Soy_ExtenseV4.csv')

Corn.extense <- merge(Corn.extense, Corn.intense[, c(1, 3, 15)], by = c('Year', 'OverallANSI'), 
                      all.x = TRUE)

Corn.median  <- data.frame(Corn.extense %>% group_by(.dots = c('Year', 'USDAagregion.x')) %>%
                             summarise(GM = median(GM_total, na.rm = TRUE)))

for(i in 1:nrow(Corn.extense)) {
  
  if(is.na(Corn.extense$GM_total[i]) & Corn.extense$Year[i] >= 1997) {
    
    Corn.extense$GM_total[i]       <- Corn.median[which(Corn.median$Year == Corn.extense$Year[i] & Corn.median$USDAagregion.x == Corn.extense$USDAagregion.x[i]), 3]
      }
  
  print(i)
  
}

Corn.extense$GM_total <- ifelse(is.na(Corn.extense$GM_total), 0, Corn.extense$GM_total)




### Soy

Soy.extense <- merge(Soy.extense, Soy.intense[, c(1, 3, 12)], by = c('Year', 'OverallANSI'), 
                      all.x = TRUE)

Soy.median  <- data.frame(Soy.extense %>% group_by(.dots = c('Year', 'USDAagregion.x')) %>%
                             summarise(GM = median(GM, na.rm = TRUE)))

for(i in 1:nrow(Soy.extense)) {
  
  if(is.na(Soy.extense$GM[i]) & Soy.extense$Year[i] >= 1997) {
    
    Soy.extense$GM[i]       <- Soy.median[which(Soy.median$Year == Soy.extense$Year[i] & Soy.median$USDAagregion.x == Soy.extense$USDAagregion.x[i]), 3]
  }
  
  print(i)
  
}

Soy.extense$GM <- ifelse(is.na(Soy.extense$GM), 0, Soy.extense$GM)

Soy.extense$GM.Soy <- Soy.extense$GM
Soy.extense        <- Soy.extense[, -23]

Corn.extense$GM.Corn <- Corn.extense$GM_total
Corn.extense         <- Corn.extense[, -23]


setwd('E:/Modules/Dissertation/Data/FinalCsvs/Extense')

write.csv(Corn.extense, 'Corn_ExtenseV5.csv', row.names = FALSE)
write.csv(Soy.extense, 'Soy_ExtenseV5.csv', row.names = FALSE)



