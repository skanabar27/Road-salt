# Data analysis for synoptic lakes dataset

library(dplyr)
library(ggplot2)
library(plotly)
library(plyr)
library(stars)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(datarium)

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



# statistical tests
# conductivity over time
# identify outliers
synoptic_conductivity %>% identify_outliers(Conductivity)
# Whimsical 1991 is an outlier, but not extreme

synoptic_conductivity_1980 %>% identify_outliers(Conductivity)
# Frog Pond 1980, is an outlier, but not extreme

synoptic_conductivity_1991 %>% identify_outliers(Conductivity)

synoptic_conductivity_2000 %>% identify_outliers(Conductivity)

# check for Normality
synoptic_conductivity %>% shapiro_test(Conductivity)
# p<0.001, cannot assume normality

synoptic_conductivity_1980 %>% shapiro_test(Conductivity)
# p<0.001, cannot assume normality

synoptic_conductivity_1991 %>% shapiro_test(Conductivity)
# p<0.001, cannot assume normality

synoptic_conductivity_2000 %>% shapiro_test(Conductivity)
# p=0.00325, cannot assume normality

# median and IQR (for Wilcoxon test)
synoptic_conductivity %>% get_summary_stats(Conductivity, type = "median_iqr")

bxp_cond <- ggboxplot(
  synoptic_conductivity$Conductivity, width = 0.5, add = c("mean", "jitter"), 
  ylab = "Conductivity (µS/cm)", xlab = FALSE
)
bxp_cond

gghistogram(synoptic_conductivity, x = "Conductivity", y = "..density..", 
            fill = "steelblue",bins = 30, add_density = TRUE)
# distribution is not symmetrical, so will try sign test

synoptic_conductivity_1980 %>% get_summary_stats(Conductivity, type = "median_iqr")

bxp_cond_1980 <- ggboxplot(
  synoptic_conductivity_1980$Conductivity, width = 0.5, add = c("mean", "jitter"), 
  ylab = "Conductivity (µS/cm)", xlab = FALSE
)
bxp_cond_1980

gghistogram(synoptic_conductivity_1980, x = "Conductivity", y = "..density..", 
            fill = "steelblue",bins = 30, add_density = TRUE)
# distribution is not symmetrical, so will try sign test

# Sign Test
synoptic_conductivity %>%
  group_by(Year) %>%
  get_summary_stats(Conductivity, type = "median_iqr")

bxp_c_sign <- ggpaired(synoptic_conductivity, x = "Year", y = "Conductivity", 
                order = c("1980", "1991", "2000"),
                ylab = "Conductivity (µS/cm)", xlab = "Year")
bxp_c_sign

stat.test <- synoptic_conductivity  %>%
  sign_test(Conductivity ~ Year) %>%
  add_significance()
stat.test
# there is a significant difference (p<0.001) between conductivity in 1980, 1991, and 2000



# Cl over time
synoptic_Cl %>% identify_outliers(Cl)
# Frenchman 2011 is an extreme outlier

synoptic_Cl_1980 %>% identify_outliers(Cl)
# Chocolate and Frog Pond 1980, are outliers, but not extreme

synoptic_Cl_1991 %>% identify_outliers(Cl)

synoptic_Cl_2000 %>% identify_outliers(Cl)

synoptic_Cl_2011 %>% identify_outliers(Cl)
# Frenchman 2011 is an extreme outlier

# check for Normality
synoptic_Cl %>% shapiro_test(Cl)
# p<0.001, cannot assume normality

synoptic_Cl_1980 %>% shapiro_test(Cl)
# p<0.001, cannot assume normality

synoptic_Cl_1991 %>% shapiro_test(Cl)
# p<0.001, cannot assume normality

synoptic_Cl_2000 %>% shapiro_test(Cl)
# p=0.00184, cannot assume normality

synoptic_Cl_2011 %>% shapiro_test(Cl)
# p<0.001, cannot assume normality

# median and IQR (for Wilcoxon test)
synoptic_Cl %>% get_summary_stats(Cl, type = "median_iqr")

bxp_Cl <- ggboxplot(
  synoptic_Cl$Cl, width = 0.5, add = c("mean", "jitter"), 
  ylab = "Cl (mg/L)", xlab = FALSE
)
bxp_Cl

gghistogram(synoptic_Cl, x = "Cl", y = "..density..", 
            fill = "steelblue",bins = 30, add_density = TRUE)
# distribution is not symmetrical, so will try sign test

# Sign Test
synoptic_Cl %>%
  group_by(Year) %>%
  get_summary_stats(Cl, type = "median_iqr")

bxp_cl_sign <- ggpaired(synoptic_Cl, x = "Year", y = "Cl", 
                       order = c("1980", "1991", "2000", "2011"),
                       ylab = "Cl (mg/L))", xlab = "Year")
bxp_cl_sign

stat.test1 <- synoptic_Cl  %>%
  sign_test(Cl ~ Year) %>%
  add_significance()
stat.test1
# there is a significant difference (p<0.001) between Cl in 1980, 1991, 2000 and 2011
# the smallest difference is between 1991 and 2000 (p=0.013)



# Na over time
synoptic_Na %>% identify_outliers(Na)
# Frenchman 2011 is an extreme outlier

synoptic_Na_1980 %>% identify_outliers(Na)
# Chocolate and Frog Pond 1980, are outliers, but not extreme

synoptic_Na_1991 %>% identify_outliers(Na)

synoptic_Na_2000 %>% identify_outliers(Na)

synoptic_Na_2011 %>% identify_outliers(Na)
# Frenchman 2011 is an extreme outlier

# check for Normality
synoptic_Na %>% shapiro_test(Na)
# p<0.001, cannot assume normality

synoptic_Na_1980 %>% shapiro_test(Na)
# p<0.001, cannot assume normality

synoptic_Na_1991 %>% shapiro_test(Na)
# p<0.001, cannot assume normality

synoptic_Na_2000 %>% shapiro_test(Na)
# p=0.00173, cannot assume normality

synoptic_Na_2011 %>% shapiro_test(Na)
# p<0.001, cannot assume normality

# median and IQR (for Wilcoxon test)
synoptic_Na %>% get_summary_stats(Na, type = "median_iqr")

bxp_Na <- ggboxplot(
  synoptic_Na$Na, width = 0.5, add = c("mean", "jitter"), 
  ylab = "Na (mg/L)", xlab = FALSE
)
bxp_Na

gghistogram(synoptic_Na, x = "Na", y = "..density..", 
            fill = "steelblue",bins = 30, add_density = TRUE)
# distribution is not symmetrical, so will try sign test

# Sign Test
synoptic_Na %>%
  group_by(Year) %>%
  get_summary_stats(Na, type = "median_iqr")

bxp_na_sign <- ggpaired(synoptic_Na, x = "Year", y = "Na", 
                        order = c("1980", "1991", "2000", "2011"),
                        ylab = "Na (mg/L))", xlab = "Year")
bxp_na_sign

stat.test2 <- synoptic_Na  %>%
  sign_test(Na ~ Year) %>%
  add_significance()
stat.test2
# there is a significant difference (p<0.001) between Na in 1980, 1991, 2000 and 2011
# the smallest difference is between 1991 and 2000 (p=0.029)



# change between Na and Cl? paired t-test







