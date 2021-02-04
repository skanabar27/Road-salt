# Data analysis for Lemont 

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

LLDL01 <- read.csv("LLDL-01.csv")

# conductivity
LL_cond <- LLDL01 %>%
  filter("")

# Conductivty (µS/cm), Cl (mg/L) and Na (mg/L)
# only at depth of 0.5
### figure out date formatting first
                                                #create blocks by season?



LLG_Sites <- read.csv("LLG_Sites.csv")

# histograms
# by year
ggplot(data = LLG_Sites, aes(x = Cond, fill = Year)) +
  geom_histogram(binwidth = 50) + 
  facet_wrap(~ Year) +
  labs(x = "Conductivity (µS/cm)",
       y = "Frequency",
       title = "Histogram of Lemont Lake Conductivity by Year") + 
  theme_bw()

# by month
ggplot(data = LLG_Sites, aes(x = Cond, fill = Month)) +
  geom_histogram(binwidth = 50) + 
  facet_wrap(~ Month) +
  labs(x = "Conductivity (µS/cm)",
       y = "Frequency",
       title = "Histogram of Lemont Lake Conductivity by Month") + 
  theme_bw()
                                                                      # change order of months!!


# conductivity by year
LLG_cond_year <- LLG_Sites %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(Mean_Conductivity = mean(Cond), SD_Conductivity = sd(Cond))

# count per year
LLG_cond_year_count <- LLG_Sites %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(count = length(Cond))
# very unequal....

# without sd
png("hw cond year.png", units="mm", width=147, height=100, res=300)
ggplot(LLG_cond_year) +
  geom_line(aes(x = Year, y = Mean_Conductivity)) +
  theme_classic() +
  labs(x = "\n Year",
       y = "Mean Conductivity (µS/cm)\n",
       title = "Mean Conductivity in Lemont Lake over time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
dev.off()

# with sd
png("hw cond year sd.png", units="mm", width=147, height=100, res=300)
ggplot(LLG_cond_year) +
  geom_line(aes(x = Year, y = Mean_Conductivity)) +
  theme_classic() +
  labs(x = "\n Year",
       y = "Mean Conductivity (µS/cm)\n",
       title = "Mean Conductivity in Lemont Lake over time") +
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



# conductivity by month
LLG_Sites$Month <- factor(LLG_Sites$Month, levels = month.abb)

LLG_cond_month <- LLG_Sites %>%
  dplyr::group_by(Month) %>%
  dplyr::summarise(Mean_Conductivity = mean(Cond), SD_Conductivity = sd(Cond)) 

# count per month
LLG_cond_month_count <- LLG_Sites %>%
  dplyr::group_by(Month) %>%
  dplyr::summarise(count = length(Cond))
# ~20-32 samples, except Feb only has 9

# without sd
png("hw cond month.png", units="mm", width=147, height=100, res=300)
ggplot(LLG_cond_month) +
  geom_line(aes(x = Month, y = Mean_Conductivity, group = 1)) +
  theme_classic() +
  labs(x = "\n Month",
       y = "Mean Conductivity (µS/cm)\n",
       title = "Mean Conductivity in Lemont Lake per month") +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
dev.off()

# with sd
png("hw cond month sd.png", units="mm", width=147, height=100, res=300)
ggplot(LLG_cond_month) +
  geom_line(aes(x = Month, y = Mean_Conductivity, group = 1)) +
  theme_classic() +
  labs(x = "\n Month",
       y = "Mean Conductivity (µS/cm)\n",
       title = "Mean Conductivity in Lemont Lake per month") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(mapping = aes(x = Month,
                              ymin = Mean_Conductivity-SD_Conductivity,
                              ymax = Mean_Conductivity+SD_Conductivity), width = 0.2) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
dev.off()



# statistical tests
# conductivity across LLG sites
# identify outliers
LLG_Sites %>% identify_outliers(Cond)
# LLG-03 Jan2010/Mar2012/Mar2016 are outliers, but not extreme

# check for Normality
LLG_Sites %>% shapiro_test(Cond)
# p<0.001, cannot assume normality

# median and IQR (for Wilcoxon test)
LLG_Sites %>% get_summary_stats(Cond, type = "median_iqr")

bxp_LLG <- ggboxplot(
  LLG_Sites$Cond, width = 0.5, add = c("mean", "jitter"), 
  ylab = "Conductivity (µS/cm)", xlab = FALSE
)
bxp_LLG

gghistogram(LLG_Sites, x = "Cond", y = "..density..", 
            fill = "steelblue",bins = 30, add_density = TRUE)
# distribution is not symmetrical, so will try sign test

# Sign Test
                                        # variables do not have the same length

LLG_Sites %>%
  group_by(Sample.Site.ID) %>%
  get_summary_stats(Cond, type = "median_iqr")

bxp_llg_sign <- ggpaired(LLG_Sites, x = "Sample.Site.ID", y = "Cond", 
                       order = c("LLG-01", "LLG-02", "LLG-03"),
                       ylab = "Conductivity (µS/cm)", xlab = "Site")
bxp_llg_sign

stat.test <- LLG_Sites  %>%
  sign_test(Cond ~ Sample.Site.ID) %>%
  add_significance()
stat.test
# there is a significant difference (p<0.001) between conductivity in 1980, 1991, and 2000



# statistical tests
# conductivity over year
# identify outliers
#LLG_Sites %>% identify_outliers(Cond)
# Whimsical 1991 is an outlier, but not extreme

#synoptic_conductivity_1980 %>% identify_outliers(Conductivity)
# Frog Pond 1980, is an outlier, but not extreme

# check for Normality
#synoptic_conductivity %>% shapiro_test(Conductivity)
# p<0.001, cannot assume normality

# median and IQR (for Wilcoxon test)
#synoptic_conductivity %>% get_summary_stats(Conductivity, type = "median_iqr")

#bxp_llg_year <- ggboxplot(
  #LLG_Sites$Cond, width = 0.5, add = c("mean", "jitter"), 
  #ylab = "Conductivity (µS/cm)", xlab = FALSE)
#bxp_cond

#gghistogram(LLG_Sites, x = "Cond", y = "..density..", 
            #fill = "steelblue",bins = 30, add_density = TRUE)
# distribution is not symmetrical, so will try sign test

# Sign Test
LLG_Sites %>%
  group_by(Year) %>%
  get_summary_stats(Cond, type = "median_iqr")

bxp_llg_year_sign <- ggpaired(LLG_Sites, x = "Year", y = "Cond", 
                       order = c(2009:2020),
                       ylab = "Conductivity (µS/cm)", xlab = "Year")
bxp_llg_year_sign

stat.test <- synoptic_conductivity  %>%
  sign_test(Conductivity ~ Year) %>%
  add_significance()
stat.test
# there is a significant difference (p<0.001) between conductivity in 1980, 1991, and 2000



# statistical tests
# conductivity by month
# identify outliers
#synoptic_conductivity %>% identify_outliers(Conductivity)
# Whimsical 1991 is an outlier, but not extreme

# check for Normality
#synoptic_conductivity %>% shapiro_test(Conductivity)
# p<0.001, cannot assume normality

# median and IQR (for Wilcoxon test)
#synoptic_conductivity %>% get_summary_stats(Conductivity, type = "median_iqr")

#bxp_cond <- ggboxplot(
  #synoptic_conductivity$Conductivity, width = 0.5, add = c("mean", "jitter"), 
  #ylab = "Conductivity (µS/cm)", xlab = FALSE)
#bxp_cond

#gghistogram(synoptic_conductivity, x = "Conductivity", y = "..density..", 
            #fill = "steelblue",bins = 30, add_density = TRUE)
# distribution is not symmetrical, so will try sign test

# Sign Test
LLG_Sites %>%
  group_by(Month) %>%
  get_summary_stats(Cond, type = "median_iqr")

bxp_llg_month_sign <- ggpaired(LLG_Sites, x = "Month", y = "Cond", 
                       order = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                       ylab = "Conductivity (µS/cm)", xlab = "Year")
bxp_llg_month_sign

stat.test <- synoptic_conductivity  %>%
  sign_test(Conductivity ~ Year) %>%
  add_significance()
stat.test
# there is a significant difference (p<0.001) between conductivity in 1980, 1991, and 2000









