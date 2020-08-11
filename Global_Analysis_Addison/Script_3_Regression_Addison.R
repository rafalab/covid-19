########################
# Regression analyses
########################

rm(list = ls()) # Remove the environment

covid = read.csv("Gathered_Covid_data.csv")


## Add Pseudo count (Min[Deaths/Pop] / 2) to each rate (instead of removing zeros)
covid$logdp = log(covid$dp + min(covid$dp[covid$dp > 0])/2)

## Log(D/P) ~ Temp
summary(lm(covid$logdp ~ covid$Temp))

## Log(D/P) ~ Temp + Month
summary(lm(covid$logdp ~ covid$Temp + as.factor(covid$Month)))

## Log(D/P) ~ Temp + Month + GNI
summary(lm(covid$logdp ~ covid$Temp + as.factor(covid$Month) + covid$GNI))


##################################################
## Stratifying by Month and GNI PC Levels
##################################################

# Stratify according to World Bank Guidelines 
# (https://blogs.worldbank.org/opendata/new-country-classifications-income-level-2019-2020)

covid_low = covid[covid$GNI < 1026, ]
covid_low_mid = covid[(covid$GNI >= 1026) & (covid$GNI <= 3995), ]
covid_upper_mid = covid[(covid$GNI >= 3996) & (covid$GNI <= 12375), ]
covid_high = covid[covid$GNI > 12375, ]


################
## March Models
################

# March Low-Income
summary(lm(covid_low$logdp[covid_low$Month == "March_temp"] ~ covid_low$Temp[covid_low$Month == "March_temp"]))

# March Lower-Middle Income
summary(lm(covid_low_mid$logdp[covid_low_mid$Month == "March_temp"] ~ 
             covid_low_mid$Temp[covid_low_mid$Month == "March_temp"]))

# March Upper-Middle Income
summary(lm(covid_upper_mid$logdp[covid_upper_mid$Month == "March_temp"] ~ 
             covid_upper_mid$Temp[covid_upper_mid$Month == "March_temp"]))

# March High-Income
summary(lm(covid_high$logdp[covid_high$Month == "March_temp"] ~ covid_high$Temp[covid_high$Month == "March_temp"]))


################
## April Models
################

# April Low Income
summary(lm(covid_low$logdp[covid_low$Month == "April_temp"] ~ covid_low$Temp[covid_low$Month == "April_temp"]))

# April Lower-Middle Income
summary(lm(covid_low_mid$logdp[covid_low_mid$Month == "April_temp"] ~ covid_low_mid$Temp[covid_low_mid$Month == "April_temp"]))

# April Upper-Middle Income
summary(lm(covid_upper_mid$logdp[covid_upper_mid$Month == "April_temp"] ~ covid_upper_mid$Temp[covid_upper_mid$Month == "April_temp"]))

# April High-Income
summary(lm(covid_high$logdp[covid_high$Month == "April_temp"] ~ covid_high$Temp[covid_high$Month == "April_temp"]))


################
## May Models
################

# May Low-Income
summary(lm(covid_low$logdp[covid_low$Month == "May_temp"] ~ covid_low$Temp[covid_low$Month == "May_temp"]))

# May Lower-Middle Income
summary(lm(covid_low_mid$logdp[covid_low_mid$Month == "May_temp"] ~ covid_low_mid$Temp[covid_low_mid$Month == "May_temp"]))

# May Upper-Middle Income
summary(lm(covid_upper_mid$logdp[covid_upper_mid$Month == "May_temp"] ~ covid_upper_mid$Temp[covid_upper_mid$Month == "May_temp"]))

# May High-Income
summary(lm(covid_high$logdp[covid_high$Month == "May_temp"] ~ covid_high$Temp[covid_high$Month == "May_temp"]))




