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
     main = "Histogram of lake Conductivity in 1980",
     breaks = 15,
     xlim = )

hist(synoptic_conductivity_1991$Conductivity,
     xlab = "Conductivity (µS/cm)",
     main = "Histogram of lake Conductivity in 1991",
     breaks = 15)

hist(synoptic_conductivity_2000$Conductivity,
     xlab = "Conductivity (µS/cm)",
     main = "Histogram of lake Conductivity in 2000",
     breaks = 15)

# all values change over time
ggplot(synoptic_conductivity) +
  geom_point(aes(x = Year, y = Conductivity, color = Name)) +
  theme_classic() +
  labs(x = "\nYear",
       y = "Conductivity\n",
       title = "Conductivity in lakes in the HRM ")                   # not usuable

# average conductivity per year
synoptic_conductivity_mean <- synoptic_conductivity %>%
  dplyr::group_by(Year) %>%
  na.omit() %>%
  dplyr::summarise(Mean_Conductivity = mean(Conductivity), SD_Conductivity = sd(Conductivity))

# count per year
synoptic_conductivity_count <- synoptic_conductivity %>%
  dplyr::group_by(Year) %>%
  na.omit() %>%
  dplyr::summarise(count = length(Conductivity))
# 1980: 49, 1991: 49, 2000: 50

# average change over time
#without sd
png("synoptic mean cond.png", units="mm", width=147, height=100, res=300)
ggplot(synoptic_conductivity_mean) +
  geom_line(aes(x = Year, y = Mean_Conductivity)) +
  theme_classic() +
  labs(x = "\n Year",
       y = "Mean Conductivity (µS/cm)\n",
       title = "Mean Conductivity of lakes in the HRM over time") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
dev.off()

#with sd
png("synoptic mean cond year sd.png", units="mm", width=147, height=100, res=300)
ggplot(synoptic_conductivity_mean) +
  geom_line(aes(x = Year, y = Mean_Conductivity)) +
  theme_classic() +
  labs(x = "\n Year",
       y = "Mean Conductivity (µS/cm)\n",
       title = "Mean Conductivity in lakes in the HRM over time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(mapping = aes(x = Year,
                              ymin = Mean_Conductivity-SD_Conductivity,
                              ymax = Mean_Conductivity+SD_Conductivity), width = 0.2) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
dev.off()


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

synoptic_Cl_2011 <-  synoptic_Cl %>%
  filter(Year == "2011")

#Histogram of conductivity each round of sampling. Normal distribution?
hist(synoptic_Cl_1980$Cl,
     xlab = "Chloride (mg/L)",
     main = "Histogram of lake chloride concentration in 1980",
     breaks = 15)

hist(synoptic_Cl_1991$Cl,
     xlab = "Chloride (mg/L)",
     main = "Histogram of lake chloride concentration in 1991",
     breaks = 15)

hist(synoptic_Cl_2000$Cl,
     xlab = "Chloride (mg/L)",
     main = "Histogram of lake chloride concentration in 2000",
     breaks = 15)

hist(synoptic_Cl_2011$Cl,
     xlab = "Chloride (mg/L)",
     main = "Histogram of lake chloride concentration in 2011",
     breaks = 15)

# average conductivity per year
synoptic_Cl_mean <- synoptic_Cl %>%
  dplyr::group_by(Year) %>%
  na.omit() %>%
  dplyr::summarise(Mean_Cl = mean(Cl), SD_Cl = sd(Cl))

# count per year
synoptic_Cl_count <- synoptic_Cl %>%
  dplyr::group_by(Year) %>%
  na.omit() %>%
  dplyr::summarise(count = length(Cl))
# 1980: 49, 1991: 48, 2000: 51, 2011: 51

# average change over time
#without sd
png("synoptic mean Cl year.png", units="mm", width=147, height=100, res=300)
ggplot(synoptic_Cl_mean) +
  geom_line(aes(x = Year, y = Mean_Cl)) +
  theme_classic() +
  labs(x = "\n Year",
       y = "Mean Chloride (mg/L)\n",
       title = "Mean Chloride Concentration in lakes in the HRM over time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
       axis.title.x = element_text(size = 12),
       axis.title.y = element_text(size = 12),
       axis.text.x = element_text(size = 10),
       axis.text.y = element_text(size = 10))
dev.off()

#with sd
png("synoptic mean Cl year sd.png", units="mm", width=147, height=100, res=300)
ggplot(synoptic_Cl_mean) +
  geom_line(aes(x = Year, y = Mean_Cl)) +
  theme_classic() +
  labs(x = "\n Year",
       y = "Mean Chloride (mg/L)\n",
       title = "Mean Chloride Concentration in lakes in the HRM over time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(mapping = aes(x = Year,
                              ymin = Mean_Cl-SD_Cl,
                              ymax = Mean_Cl+SD_Cl), width = 0.2) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
dev.off()





# plot change in Na over time
synoptic_Na <- synoptic %>%
  select("Name", "Year", "Na")

# converting factors to class numeric
synoptic_Na$Year <- as.numeric(synoptic_Na$Year)
synoptic_Na$Na <- as.numeric(synoptic_Na$Na)

# separating by year
synoptic_Na_1980 <-  synoptic_Na %>%
  filter(Year == "1980")

synoptic_Na_1991 <-  synoptic_Na %>%
  filter(Year == "1991")

synoptic_Na_2000 <-  synoptic_Na %>%
  filter(Year == "2000")

synoptic_Na_2011 <-  synoptic_Na %>%
  filter(Year == "2011")

#Histogram of conductivity each round of sampling. Normal distribution?
hist(synoptic_Na_1980$Na,
     xlab = "Sodium (mg/L)",
     main = "Histogram of lake sodium concentration in 1980",
     breaks = 15)

hist(synoptic_Na_1991$Na,
     xlab = "Sodium (mg/L)",
     main = "Histogram of lake sodium concentration in 1991",
     breaks = 15)

hist(synoptic_Na_2000$Na,
     xlab = "Sodium (mg/L)",
     main = "Histogram of lake sodium concentration in 2000",
     breaks = 15)

hist(synoptic_Na_2011$Na,
     xlab = "Sodium (mg/L)",
     main = "Histogram of lake sodium concentrationin 2011",
     breaks = 15)

# average conductivity per year
synoptic_Na_mean <- synoptic_Na %>%
  dplyr::group_by(Year) %>%
  na.omit() %>%
  dplyr::summarise(Mean_Na = mean(Na), SD_Na = sd(Na))

# count per year
synoptic_Na_count <- synoptic_Na %>%
  dplyr::group_by(Year) %>%
  na.omit() %>%
  dplyr::summarise(count = length(Na))
# 1980: 49, 1991: 48, 2000: 51, 2011: 51

# average change over time
#without sd
png("synoptic mean Na year.png", units="mm", width=147, height=100, res=300)
ggplot(synoptic_Na_mean) +
  geom_line(aes(x = Year, y = Mean_Na)) +
  theme_classic() +
  labs(x = "\n Year",
       y = "Mean Sodium Concentration (mg/L)\n",
       title = "Mean Sodium Concentration in lakes in the HRM over time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
dev.off()

#with sd
png("synoptic mean Na year sd.png", units="mm", width=147, height=100, res=300)
ggplot(synoptic_Na_mean) +
  geom_line(aes(x = Year, y = Mean_Na)) +
  theme_classic() +
  labs(x = "\n Year",
       y = "Mean Sodium Concentration (mg/L)\n",
       title = "Mean Sodium Concentration in lakes in the HRM over time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(mapping = aes(x = Year,
                              ymin = Mean_Na-SD_Na,
                              ymax = Mean_Na+SD_Na), width = 0.2) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
dev.off()





# combined Cl and Na
synoptic_Na_Cl <- synoptic %>%
  select("Name", "Year", "Cl", "Na")

synoptic_Na_Cl$Year <- as.numeric(synoptic_Na_Cl$Year)
synoptic_Na_Cl$Cl <- as.numeric(synoptic_Na_Cl$Cl)
synoptic_Na_Cl$Na <- as.numeric(synoptic_Na_Cl$Na)

synoptic_Na_Cl_mean <- synoptic_Na_Cl %>%
  dplyr::group_by(Year) %>%
  na.omit() %>%
  dplyr::summarise(Mean_Na = mean(Na), SD_Na = sd(Na), Mean_Cl = mean(Cl), SD_Cl = sd(Cl))

# figure combined
png("synoptic mean NaCl year.png", units="mm", width=147, height=100, res=300)
ggplot(synoptic_Na_Cl_mean, aes(x = Year)) +
  geom_line(aes(y = Mean_Na, color = "mediumpurple4")) +
  geom_line(aes(y = Mean_Cl, color = "mediumpurple3")) +
  theme_classic() +
  labs(x = "\n Year",
       y = "Mean Ion Concentration (mg/L)\n",
       title = "Mean Sodium and Chloride Concentration in lakes in the HRM over time \n") +
  theme(plot.title = element_text(hjust = 0.5, size = 12)) +
  scale_color_discrete(name = "Ions", labels = c("Cl", "Na")) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
dev.off()











