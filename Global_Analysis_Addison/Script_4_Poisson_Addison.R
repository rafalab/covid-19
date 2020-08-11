#install.packages("MASS")
#install.packages("dgof")
#install.packages("readxl")
#install.packages("MBESS")
library(MASS)
library(dgof)
library(readxl)
library(MBESS)


rm(list = ls()) # Remove the environment

covid = read.csv("Gathered_Covid_data.csv") # Read-in data

## Account for countries with zero cases (add pseudo count)
covid$logdp = log(covid$dp + min(covid$dp[covid$dp > 0])/2)
covid = covid[ , -which(colnames(covid) == "March_logdc")]
covid = covid[ , -which(colnames(covid) == "April_logdc")]
covid = covid[ , -which(colnames(covid) == "May_logdc")]


## Stratify based on GNI Per Capita
covid_low = covid[covid$GNI < 1026, ]
covid_low_mid = covid[(covid$GNI >= 1026) & (covid$GNI <= 3995), ]
covid_upper_mid = covid[(covid$GNI >= 3996) & (covid$GNI <= 12375), ]
covid_high = covid[covid$GNI > 12375, ]


#############################################
## March Data Stratified by Income Level
#############################################
Ml = covid_low[covid_low$Month == "March_temp", ] # Low-Income
Mlm = covid_low_mid[covid_low_mid$Month == "March_temp", ] # Low-Middle Income
Mum = covid_upper_mid[covid_upper_mid$Month == "March_temp", ] # Upper-Middle Income
Mh = covid_high[covid_high$Month == "March_temp", ] # High Income

par(mfrow = c(2,2)) # Plot histograms
hist(Ml$logdp)
hist(Mlm$logdp)
hist(Mum$logdp)
hist(Mh$logdp)

## Negative-Binomial March
summary(glm.nb(Ml$Deaths ~ Ml$Temp + offset(log(Ml$Population)),
            data = Ml))

summary(glm.nb(Mlm$Deaths ~ Mlm$Temp + offset(log(Mlm$Population)),
            data = Mlm))

summary(glm.nb(Mum$Deaths ~ Mum$Temp + offset(log(Mum$Population)),
            data = Mum))

summary(glm.nb(Mh$Deaths ~ Mh$Temp + offset(log(Mh$Population)),
            data = Mh))


## Quasi-Poisson - March
summary(glm(Ml$Deaths ~ Ml$Temp + offset(log(Ml$Population)),
            family = quasipoisson, data = Ml))

summary(glm(Mlm$Deaths ~ Mlm$Temp + offset(log(Mlm$Population)),
            family = quasipoisson, data = Mlm))

summary(glm(Mum$Deaths ~ Mum$Temp + offset(log(Mum$Population)),
            family = quasipoisson, data = Mum))

summary(glm(Mh$Deaths ~ Mh$Temp + offset(log(Mh$Population)),
            family = quasipoisson, data = Mh))



##############################
## April Data
##############################
Al = covid_low[covid_low$Month == "April_temp", ]
Alm = covid_low_mid[covid_low_mid$Month == "April_temp", ]
Aum = covid_upper_mid[covid_upper_mid$Month == "April_temp", ]
Ah = covid_high[covid_high$Month == "April_temp", ]

par(mfrow = c(2,2)) # Plot histograms
hist(Al$logdp)
hist(Alm$logdp)
hist(Aum$logdp)
hist(Ah$logdp)

## Negative-Binomial April
summary(glm.nb(Al$Deaths ~ Al$Temp + offset(log(Al$Population)),
            data = Al))

summary(glm.nb(Alm$Deaths ~ Alm$Temp + offset(log(Alm$Population)),
            data = Alm))

summary(glm.nb(Aum$Deaths ~ Aum$Temp + offset(log(Aum$Population)),
            data = Aum))

summary(glm.nb(Ah$Deaths ~ Ah$Temp + offset(log(Ah$Population)),
               data = Ah))


## Quasi-Poisson - April
summary(glm(Al$Deaths ~ Al$Temp + offset(log(Al$Population)),
            family = quasipoisson, data = Al))

summary(glm(Alm$Deaths ~ Alm$Temp + offset(log(Alm$Population)),
            family = quasipoisson, data = Alm))

summary(glm(Aum$Deaths ~ Aum$Temp + offset(log(Aum$Population)),
            family = quasipoisson, data = Aum))

summary(glm(Ah$Deaths ~ Ah$Temp + offset(log(Ah$Population)),
            family = quasipoisson, data = Ah))



##############################
## May Data 
##############################
Mal = covid_low[covid_low$Month == "May_temp", ]
Malm = covid_low_mid[covid_low_mid$Month == "May_temp", ]
Maum = covid_upper_mid[covid_upper_mid$Month == "May_temp", ]
Mah = covid_high[covid_high$Month == "May_temp", ]


## Negative-Binomial May
summary(glm.nb(Mal$Deaths ~ Mal$Temp + offset(log(Mal$Population)),
            data = Mal))

#summary(glm.nb(Malm$Deaths ~ Malm$Temp + offset(log(Malm$Population)), #Does not run; Dr.Google couldn't help
#            data = Malm))

summary(glm.nb(Maum$Deaths ~ Maum$Temp + offset(log(Maum$Population)), 
            data = Maum))

summary(glm.nb(Mah$Deaths ~ Mah$Temp + offset(log(Mah$Population)),
            data = Mah))


## Quasi-Poisson - May
summary(glm(Mal$Deaths ~ Mal$Temp + offset(log(Mal$Population)),
            family = quasipoisson, data = Mal))

summary(glm(Malm$Deaths ~ Malm$Temp + offset(log(Malm$Population)),
            family = quasipoisson, data = Malm))

summary(glm(Maum$Deaths ~ Maum$Temp + offset(log(Maum$Population)),
            family = quasipoisson, data = Maum))

summary(glm(Mah$Deaths ~ Mah$Temp + offset(log(Mah$Population)),
            family = quasipoisson, data = Mah))

hist(Mal$logdp) # Plot histograms
hist(Malm$logdp)
hist(Maum$logdp)
hist(Mah$logdp)





