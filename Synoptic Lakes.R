# Data analysis for synoptic lakes dataset

library(dplyr)
library(ggplot2)
library(plotly)
library(plyr)
library(stars)
library(tidyr)

# add condensed dataset with salt and conductivity data
# sf library is corrupt?
synoptic <- read.csv("Synoptic salt.csv")


# plot change in conductivity over time
synoptic_conductivity <- synoptic %>%
  select("Name", "Year", "Conductivity")
  
# converting factors to class numeric
synoptic_conductivity$Year <- as.numeric(synoptic_conductivity$Year)
synoptic_conductivity$Conductivity <- as.numeric(synoptic_conductivity$Conductivity)

# separating by year
synoptic_conductivity_1980 <-  synoptic_conductivity %>%
  filter(Year == "1980")

synoptic_conductivity_1991 <-  synoptic_conductivity %>%
  filter(Year == "1991")

synoptic_conductivity_2000 <-  synoptic_conductivity %>%
  filter(Year == "2000")

#Histogram of conductivity each round of sampling. Normal distribution?
hist(synoptic_conductivity_1980$Conductivity,
     xlab = "Conductivity",
     main = "Histogram of Lake Conductivity in 1980",
     breaks = 15)

hist(synoptic_conductivity_1991$Conductivity,
     xlab = "Conductivity",
     main = "Histogram of Lake Conductivity in 1991",
     breaks = 15)

hist(synoptic_conductivity_2000$Conductivity,
     xlab = "Conductivity",
     main = "Histogram of Lake Conductivity in 2000",
     breaks = 15)

# all values change over time
ggplot(synoptic_conductivity) +
  geom_point(aes(x = Year, y = Conductivity)) +
  theme_classic() +
  labs(x = "\nYear",
       y = "Conductivity\n",
       title = "Conductivity in lakes in the HRM ") #+
  
  


# average conductivity per year
synoptic_conductivity_mean <- synoptic_conductivity %>%
  dplyr::group_by(Year) %>%
  na.omit() %>%
  dplyr::summarise(Mean_Conductivity = mean(Conductivity), SD_Conductivity = sd(Conductivity))

# average change over time
ggplot(synoptic_conductivity_mean) +
  geom_line()

















#calculate average conductivity each year
lake_conductivity_mean_1980 <- lake_conductivity %>%
  na.omit() %>%
  summarise(Mean_1980 = mean(Cond.1980), SD_1980 = sd(Cond.1980)) 

lake_conductivity_mean_1991 <- lake_conductivity %>%
  na.omit() %>%
  summarise(Mean_1991 = mean(Cond.1991), SD_1991 = sd(Cond.1991)) 

lake_conductivity_mean_2000 <- lake_conductivity %>%
  na.omit() %>%
  summarise(Mean_2000 = mean(Sp_Cond.2000), SD_2000 = sd(Sp_Cond.2000))



