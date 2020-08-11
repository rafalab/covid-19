#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("ggpubr")
#install.packages("taRifx")
#install.packages("ggrepel")
#install.packages("RColorBrewer")
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(taRifx)
library(ggrepel)
library(RColorBrewer)

rm(list = ls()) # Remove the environment

March = read.csv("March_Data.csv") # Read-in data for March

# Extract and rename vectors
March = cbind.data.frame(March$location, 
                         March$temperature, 
                         March$humidity,
                         March$casepop,
                         March$deathpopu,
                         March$cases,
                         March$deaths,
                         log(March$deathpopu),
                         March$population)

colnames(March)[1:9] = c("location", 
                          "March_temp", 
                          "March_humidity",
                          "March_cp",
                          "March_dp",
                          "March_cases",
                          "March_deaths",
                          "March_logdc",
                          "March_population")

# Repeat for April and May
April = read.csv("April_Data.csv")

April = cbind.data.frame(April$location, 
                         April$temperature, 
                         April$humidity,
                         April$casepop,
                         April$deathpopu,
                         April$cases,
                         April$deaths,
                         log(April$deathpopu),
                         April$population)

colnames(April)[1:9] = c("location", 
                          "April_temp", 
                          "April_humidity",
                          "April_cp",
                          "April_dp",
                          "April_cases",
                          "April_deaths",
                          "April_logdc",
                          "April_population")


May = read.csv("May_Data.csv")
May = cbind.data.frame(May$location, 
                       May$temperature, 
                       May$humidity,
                       May$casepop,
                       May$deathpopu,
                       May$cases,
                       May$deaths,
                       log(May$deathpopu),
                       May$population)

colnames(May)[1:9] = c("location", 
                        "May_temp", 
                        "May_humidity",
                        "May_cp",
                        "May_dp",
                        "May_cases",
                        "May_deaths",
                        "May_logdc",
                        "May_population")

covid = merge.data.frame(March, April, by = 1, no.dups = T, all = F) # Merge datasets
covid = merge.data.frame(covid, May, by = 1, no.dups = T, all = F)



#############################################################
# Clean data for GNI PC = Gross National Income Per Capita
#############################################################

library(readxl)
GNIPC <- read_excel("GNIPC.xls", col_types = c("blank", 
         "text", "blank", "text", "text", "blank", "blank", "blank", "blank", "blank", "text", 
         "text", "numeric", "text"), skip = 5)

GNIPC = GNIPC[-c(1, 191:241), -c(1, 4:7)]
GNIPC$Economy[152] = "Congo"
GNIPC$Economy[185] = "Democratic Republic of Congo"
GNIPC$Economy[136] = "Egypt"
GNIPC$Economy[176] = "Gambia"
GNIPC$Economy[175] = "Guinea Bissau"
GNIPC$Economy[15] = "Hong Kong"
GNIPC$Economy[98] = "Iran"
GNIPC$Economy[164] = "Kyrgyzstan"
GNIPC$Economy[139] = "Laos"
GNIPC$Economy[69] = "Russia"
GNIPC$Economy[47] = "Slovakia"


# Add GNI PC to dataset
covid = merge.data.frame(covid, GNIPC, by = 1, no.dups = T, all = F) # Merge data
covid$`(US dollars)` = as.numeric(covid$`(US dollars)`) # Rename variables
colnames(covid)[ncol(covid)] = "GNI" 


# Make data "tidy" (collapse columns into observations)
temp = data.frame(Population = gather(covid, "Month", "Population", 
                                      c(which(colnames(covid) == "March_population"), 
                                        which(colnames(covid) == "April_population"), 
                                        which(colnames(covid) == "May_population")))$Population)

temp2 = data.frame(Deaths = gather(covid, "Month", "Deaths", 
                                   c(which(colnames(covid) == "March_deaths"), 
                                     which(colnames(covid) == "April_deaths"), 
                                     which(colnames(covid) == "May_deaths")))$Deaths)

covid = cbind.data.frame(gather(covid, "Month", "Temp", 
                                c(which(colnames(covid) == "March_temp"), 
                                  which(colnames(covid) == "April_temp"), 
                                  which(colnames(covid) == "May_temp"))), 
                         gather(covid, "Month2", "logdp", 
                                c(which(colnames(covid) == "March_logdc"), 
                                  which(colnames(covid) == "April_logdc"), 
                                  which(colnames(covid) == "May_logdc")))$logdp)

colnames(covid)[ncol(covid)] = "logdp" # Rename variables

covid = cbind.data.frame(covid, temp) # Bind dataframes
covid = cbind.data.frame(covid, temp2)

covid$dp = covid$Deaths / covid$Population # Compute Deaths/Population

write.csv(covid, "Gathered_Covid_data.csv") # Save dataset


# Change to factor to override alphabetical ordering (i.e. March < April < May)
covid$Month = factor(covid$Month, levels = c("March_temp", "April_temp", "May_temp"), 
                     labels = c("March", "April", "May"), ordered = T)                                              


# Plot Deaths/Population
ggplot(covid, aes(Temp, dp, label = location)) +
  geom_point(aes(color = Month)) +
  ggtitle("Deaths/Pop vs Temp") + 
  ylab("Deaths / Population") +
  xlab("Temperature (°C)") +
  scale_color_discrete(labels = c("March", "April", "May")) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text_repel(segment.size = 0, data = subset(covid, 
      dp > quantile(dp, seq(from = .01, to = 1, by = .01), na.rm = T)[99]))


# Plot Log[Deaths/Pop]
ggplot(covid, aes(Temp, logdp, label = location)) +
  geom_point(aes(color = Month)) +
  ggtitle("Log[Deaths/Pop] vs Temp") + 
  ylab("Log[Deaths/Population]") +
  xlab("Temperature (°C)") +
  scale_color_discrete(labels = c("March", "April", "May")) +
  theme(plot.title = element_text(hjust = 0.5))


# Add regression lines
ggplot(covid, aes(Temp, logdp, label = location)) +
  geom_point(aes(color = Month)) +
  ggtitle("Log[Deaths/Pop] vs Temp") + 
  ylab("Log[Deaths/Population]") +
  xlab("Temperature (°C)") +
  scale_color_discrete(labels = c("March", "April", "May")) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_smooth(aes(color = Month), method = "lm", se = F)
