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
     xlab = "Conductivity (µS/cm)",
     main = "Histogram of Lake Conductivity in 1980",
     breaks = 15)

hist(synoptic_conductivity_1991$Conductivity,
     xlab = "Conductivity (µS/cm)",
     main = "Histogram of Lake Conductivity in 1991",
     breaks = 15)

hist(synoptic_conductivity_2000$Conductivity,
     xlab = "Conductivity (µS/cm)",
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
  geom_line(aes(x = Year, y = Mean_Conductivity)) +
  theme_classic() +
  labs(x = "\n Year",
       y = "Mean Conductivity (µS/cm)\n",
       title = "Mean Conductivity of lakes in the HRM over time") +
  theme(plot.title = element_text(hjust = 0.5))





# plot change in Cl over time
synoptic_Cl <- synoptic %>%
  select("Name", "Year", "Cl")

# converting factors to class numeric
synoptic_Cl$Year <- as.numeric(synoptic_Cl$Year)
synoptic_Cl$Cl <- as.numeric(synoptic_Cl$Cl)

# separating by year
synoptic_Cl_1980 <-  synoptic_Cl %>%
  filter(Year == "1980")

synoptic_Cl_1991 <-  synoptic_Cl %>%
  filter(Year == "1991")

synoptic_Cl_2000 <-  synoptic_Cl %>%
  filter(Year == "2000")

#Histogram of conductivity each round of sampling. Normal distribution?
hist(synoptic_Cl_1980$Cl,
     xlab = "Cl (mg/L)",
     main = "Histogram of Lake Chloride in 1980",
     breaks = 15)

hist(synoptic_conductivity_1991$Conductivity,
     xlab = "Cl (mg/L)",
     main = "Histogram of Lake Chloride in 1991",
     breaks = 15)

hist(synoptic_conductivity_2000$Conductivity,
     xlab = "Cl (mg/L)",
     main = "Histogram of Lake Chloride in 2000",
     breaks = 15)

# all values change over time
ggplot(synoptic_Cl) +
  geom_point(aes(x = Year, y = Cl)) +
  theme_classic() +
  labs(x = "\nYear",
       y = "Chloride (mg/L)\n",
       title = "Chloride levels in lakes in the HRM ") #+

# average conductivity per year
synoptic_Cl_mean <- synoptic_Cl %>%
  dplyr::group_by(Year) %>%
  na.omit() %>%
  dplyr::summarise(Mean_Cl = mean(Cl), SD_Cl = sd(Cl))

# average change over time
ggplot(synoptic_conductivity_mean) +
  geom_line(aes(x = Year, y = Mean_Conductivity)) +
  theme_classic() +
  labs(x = "\n Year",
       y = "Mean Conductivity (µS/cm)\n",
       title = "Mean Conductivity of lakes in the HRM over time") +
  theme(plot.title = element_text(hjust = 0.5))















