# Data analysis for synoptic lakes dataset

library(dplyr)
library(ggplot2)
library(plotly)
library(plyr)
#library(sf)
library(stars)
library(tidyr)

# add condensed dataset with salt and conductivity data
# sf library is corrupt?
lake <- read.csv("Synoptic salt.csv")


# plot change in conductivity over time
lake_conductivity <- lake %>%
  select("Name", "Cond.1980", "Cond.1991", "Sp_Cond.2000")

# converting factors to class numeric
lake_conductivity$Cond.1980 <- as.numeric(lake_conductivity$Cond.1980)
lake_conductivity$Cond.1991 <- as.numeric(lake_conductivity$Cond.1991)
lake_conductivity$Sp_Cond.2000 <- as.numeric(lake_conductivity$Sp_Cond.2000)

#Histogram of conductivity each round of sampling. Normal distribution?
hist(lake_conductivity$Cond.1980,
     xlab = "Conductivity",
     main = "Histogram of Lake Conductivity in 1980",
     breaks = 15)

hist(lake_conductivity$Cond.1991,
     xlab = "Conductivity",
     main = "Histogram of Lake Conductivity in 1991",
     breaks = 15)

hist(lake_conductivity$Sp_Cond.2000,
     xlab = "Conductivity",
     main = "Histogram of Lake Conductivity in 2000",
     breaks = 15)

# all values change over time
ggplot(lake_conductivity) +
  geom_line(aes(x = Year))



# average change over time



