########################
# Global COVID Data
########################
#install.packages("tidyverse")
#install.packages("dplyr")
library(tidyverse)
library(dplyr)

rm(list = ls()) # Remove the environment

func = function(x) { # function to sum while removing NAs; used in aggregate() function
  return(sum(x, na.rm = T))
}


covid_cases = read.csv("owid-covid-data.csv") # Read-in data
# View(covid_cases)


# Edit "date" variable to help filter by month
covid_cases$date = gsub("2019-", "", covid_cases$date) # Remove year and hyphen
covid_cases$date = gsub("2020-", "", covid_cases$date) # Remove year and hyphen
covid_cases$date = substr(covid_cases$date, 1, nchar(covid_cases$date) - 3) # remove day of month and hyphen


# Create March dataset
March = covid_cases[covid_cases$date == "03", ] # Take all data from March (third month)
march_cases = aggregate(March$total_cases, March["location"], FUN = func) # Find total number of cases in each country during March
march_deaths = aggregate(March$total_deaths, March["location"], FUN = func) # Find total number of deaths in each country during March

march_pop = March[!duplicated(March$location), ] # Extract population for each country
march_iso = March[!duplicated(March$iso_code), ] # Extract ISO code for each country

# Create new dataframe with vectors of location, ISO code, number of cases/deaths, and population
March_2 = cbind.data.frame(location = march_cases$location, 
                           iso_code = march_iso$iso_code,
                           cases = march_cases$x,
                           deaths = march_deaths$x,
                           population = march_pop$population)

March_2 = March_2[March_2$population > 1000000, ] # Remove countries with Population < 1 million


######################################
# Repeat steps above for April and May
######################################

# April
April = covid_cases[covid_cases$date == "04", ] 
april_cases = aggregate(April$total_cases, April["location"], FUN = func)
april_deaths = aggregate(April$total_deaths, April["location"], FUN = func)
april_pop = April[!duplicated(April$location), ]
april_iso = April[!duplicated(April$iso_code), ]

April_2 = cbind.data.frame(location = april_cases$location,
                           iso_code = april_iso$iso_code,
                           cases = april_cases$x,
                           deaths = april_deaths$x,
                           population = april_pop$population)

April_2 = April_2[April_2$population > 1000000, ] # Remove countries with Population < 1 million


# May
May = covid_cases[covid_cases$date == "05", ]
may_cases = aggregate(May$total_cases, May["location"], FUN = func)
may_deaths = aggregate(May$total_deaths, May["location"], FUN = func)
may_pop = May[!duplicated(May$location), ]
may_iso = May[!duplicated(May$iso_code), ]

May_2 = cbind.data.frame(location = may_cases$location,
                         iso_code = may_iso$iso_code,
                         cases = may_cases$x,
                         deaths = may_deaths$x,
                         population = may_pop$population)

May_2 = May_2[May_2$population > 1000000, ] # Remove countries with Population < 1 million


###############################################################
################ Avg Temperature Per Country ####################
###############################################################

weather = read.csv("Weather2019.csv") # Read-in data

weather = weather[ , -c(2:4)] # Remove extraneous columns
weather = weather[ , c(1:4)] # Retain pertinent columns (March, April, and May AVG. temperature)

# Convert from Fahrenheit to Celcius
for (i in 2:ncol(weather)) {
  weather[, i] = (5/9) * (weather[, i] - 32)
}

# Edit data to help facilitate merging with COVID dataset
weather$CTRY = gsub("_", " ", weather$CTRY)
weather$CTRY = gsub("-", " ", weather$CTRY)
weather$CTRY = gsub("DR ", "Democratic Republic of ", weather$CTRY)
weather$CTRY = gsub("Central African Rep", "Central African Republic", weather$CTRY)
weather$CTRY = gsub("Bosnia Herzegovinia", "Bosnia and Herzegovina", weather$CTRY)
weather$CTRY = gsub("Puerto Rica", "Puerto Rico", weather$CTRY)
weather$CTRY[which(weather$CTRY == "USA")] = "United States"


# Merge temperature and COVID data
March_Cases_Weather = merge.data.frame(March_2, weather[ , c(1:2)], by = 1, no.dups = T, all = F) 
colnames(March_Cases_Weather)[6] = "temperature"

April_Cases_Weather = merge.data.frame(April_2, weather[ , c(1,3)], by = 1, no.dups = T, all = F)
colnames(April_Cases_Weather)[6] = "temperature"

May_Cases_Weather = merge.data.frame(May_2, weather[ , c(1,4)], by = 1, no.dups = T, all = F)
colnames(May_Cases_Weather)[6] = "temperature"



####################################################
# Repeat steps above for Absolute Humidity data
####################################################

humidity = read.csv("humidity.csv")

humidity = humidity[ , -c(2:4)] # Clean humidity data like with temperature
humidity$CTRY = weather$CTRY # Copy over country variable from Weather (no longer need to edit country names)
humidity = humidity[ , c(1:4)]

March_Cases_Weather = merge.data.frame(March_Cases_Weather, humidity[ , c(1:2)], by = 1, no.dups = T, all = F) 
colnames(March_Cases_Weather)[7] = "humidity"

April_Cases_Weather = merge.data.frame(April_Cases_Weather, humidity[ , c(1,3)], by = 1, no.dups = T, all = F)
colnames(April_Cases_Weather)[7] = "humidity"

May_Cases_Weather = merge.data.frame(May_Cases_Weather, humidity[ , c(1,4)], by = 1, no.dups = T, all = F)
colnames(May_Cases_Weather)[7] = "humidity"


# Calculate number of Cases divided by Population
March_Cases_Weather$casepop = March_Cases_Weather$cases / March_Cases_Weather$population
April_Cases_Weather$casepop = April_Cases_Weather$cases / April_Cases_Weather$population
May_Cases_Weather$casepop = May_Cases_Weather$cases / May_Cases_Weather$population

# Calculate number of Deaths divided by Population
March_Cases_Weather$deathpopu = March_Cases_Weather$deaths / March_Cases_Weather$population
April_Cases_Weather$deathpopu = April_Cases_Weather$deaths / April_Cases_Weather$population
May_Cases_Weather$deathpopu = May_Cases_Weather$deaths / May_Cases_Weather$population


# Take log of cases/pop and deaths/pop
March_Cases_Weather$logcasespop = log(March_Cases_Weather$casepop)
April_Cases_Weather$logcasespop = log(April_Cases_Weather$casepop)
May_Cases_Weather$logcasespop = log(May_Cases_Weather$casepop)

March_Cases_Weather$logdeathspop = log(March_Cases_Weather$deathpopu)
April_Cases_Weather$logdeathspop = log(April_Cases_Weather$deathpopu)
May_Cases_Weather$logdeathspop = log(May_Cases_Weather$deathpopu)

# Save datasets
write.csv(March_Cases_Weather, "March_Data.csv")
write.csv(April_Cases_Weather, "April_Data.csv")
write.csv(May_Cases_Weather, "May_Data.csv")






