
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
colnames(Corn.frame)[22]          <- as.character(Corn_harvested$Data.Item[1])
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

extra_commods              <- list(Cotton_planted, Rice, Sorghum, Sugar_Beet, Wheat)
extra_commods              <- rbind.fill(extra_commods)

extra_commods$Value        <- as.numeric(gsub(',', '', extra_commods$Value))

extra_commods$County.ANSI  <- formatC(extra_commods$County.ANSI, width = 3, format = "d", flag = "0")
extra_commods$County.ANSI  <- assign_other_county(extra_commods)
extra_commods$OverallANSI  <- paste0(extra_commods$State.ANSI, extra_commods$County.ANSI)
extra_commods$USDAagregion <- map_agregion(extra_commods)

extra_commods              <- extra_commods[, -c(4, 12:15, 18:19, 21)]

### Produced


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

setwd('C:/Users/Oli/Documents/Modules/Dissertation/Data/Other/Intensification/Fertiliser by Commod_Processed')

