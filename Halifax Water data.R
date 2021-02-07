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

#LLDL01 <- read.csv("LLDL-01.csv")

# conductivity
#LL_cond <- LLDL01 %>% filter("")

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
# 2009:9, 2010:31, 2011:32, 2012:36, 2013:28, 2014:18, 2015:21, 2016:15, 2017:30, 2018:34, 2019:17, 2020:4

# without sd
png("hw cond year.png", units="mm", width=147, height=100, res=300)
ggplot(LLG_cond_year) +
  geom_line(aes(x = Year, y = Mean_Conductivity)) +
  theme_classic() +
  labs(x = "\n Year",
       y = "Mean Conductivity (µS/cm)\n",
       title = "Mean Conductivity in Lemont watershed over time") +
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
       title = "Mean Conductivity in Lemont watershed over time") +
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
# Jan:21, Feb:9, Mar:22, Apr:21, May:28, Jun:21, Jul:27, Aug:20, Sep:29, Oct:24, Nov:32, Dec:21

# without sd
png("hw cond month.png", units="mm", width=147, height=100, res=300)
ggplot(LLG_cond_month) +
  geom_line(aes(x = Month, y = Mean_Conductivity, group = 1)) +
  theme_classic() +
  labs(x = "\n Month",
       y = "Mean Conductivity (µS/cm)\n",
       title = "Mean Conductivity in Lemont watershed per month") +
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
       title = "Mean Conductivity in Lemont watershed per month") +
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
# n=275, median=69, iqr=127

png("hw cond boxplot.png", units="mm", width=147, height=100, res=300)
bxp_LLG <- ggboxplot(
  LLG_Sites$Cond, width = 0.5, add = c("mean", "jitter"), 
  ylab = "Conductivity (µS/cm)", xlab = FALSE
)
bxp_LLG
dev.off()

gghistogram(LLG_Sites, x = "Cond", y = "..density..", 
            fill = "steelblue",bins = 30, add_density = TRUE)
# distribution is not symmetrical, so will try sign test


# Sign Test
                                        # variables do not have the same length

LLG_Sites %>%
  group_by(Sample.Site.ID) %>%
  get_summary_stats(Cond, type = "median_iqr")
# LLG-01 n=86, median=42.9, iqr=7
# LLG-02 n=97, median=67, iqr=11
# LLG-03 n=92, median=228, iqr=96.5

bxp_llg_sign <- ggpaired(LLG_Sites, x = "Sample.Site.ID", y = "Cond", 
                       order = c("LLG-01", "LLG-02", "LLG-03"),
                       ylab = "Conductivity (µS/cm)", xlab = "Site")
bxp_llg_sign

stat.test <- LLG_Sites  %>%
  sign_test(Cond ~ Sample.Site.ID) %>%
  add_significance()
stat.test
# 



# statistical tests
# conductivity over year

LLG_year <- LLG_Sites %>%
  select(Year, Cond)

LLG_Sites %>% identify_outliers(Cond)
# LLG-03 Jan2010/Mar2012/Mar2016 are outliers, but not extreme

# check for Normality
LLG_Sites %>% shapiro_test(Cond)
# p<0.001, cannot assume normality


# median and IQR (for Wilcoxon test)
LLG_year %>% get_summary_stats(Cond, type = "median_iqr")
# n=275, median=69, iqr=127

bxp_llg_year <- ggboxplot(
  LLG_year$Cond, width = 0.5, add = c("mean", "jitter"), 
  ylab = "Conductivity (µS/cm)", xlab = FALSE)
bxp_llg_year

#gghistogram(LLG_year, x = "Cond", y = "..density..", 
            #fill = "steelblue",bins = 30, add_density = TRUE)
# distribution is not symmetrical, so will try sign test


# Sign Test
LLG_year %>%
  group_by(Year) %>%
  get_summary_stats(Cond, type = "median_iqr")
# 2009 n=9, median=71, iqr=91
# 2010 n=31, median=70, iqr=102
# 2011 n=32, median=69, iqr=75.8
# 2012 n=36, median=62, iqr=135
# 2013 n=28, median=62.5, iqr=132
# 2014 n=18, median=68.5, iqr=109
# 2015 n=21, median=79, iqr=141
# 2016 n=15, median=81, iqr=140
# 2017 n=30, median=73, iqr=108
# 2018 n=34, median=70.6, iqr=108
# 2019 n=17, median=58.8, iqr=173
# 2020 n=4, median=142, iqr=169

bxp_llg_year_sign <- ggpaired(LLG_Sites, x = "Year", y = "Cond", 
                       order = c(2009:2020),
                       ylab = "Conductivity (µS/cm)", xlab = "Year")
bxp_llg_year_sign

stat.test <- synoptic_conductivity  %>%
  sign_test(Conductivity ~ Year) %>%
  add_significance()
stat.test
# 



# statistical tests
# conductivity by month


# Sign Test
LLG_Sites %>%
  group_by(Month) %>%
  get_summary_stats(Cond, type = "median_iqr")
# Jan n=21, median=79, iqr=174
# Feb n=9, median=79.3, iqr=233
# Mar n=22, median=62.5, iqr=192
# Apr n=21, median=64.4, iqr=176
# May n=28, median=67.0, iqr=170
# Jun n=21, median=68, iqr=168
# Jul n=27, median=70, iqr=64.7
# Aug n=20, median=68.7, iqr=106
# Sep n=29, median=74, iqr=70
# Oct n=24, median-68.5, iqr=100
# Nov n=32, median=70.5, iqr-101
# Dec n=21, median=70, iqr=104

bxp_llg_month_sign <- ggpaired(LLG_Sites, x = "Month", y = "Cond", 
                       order = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
                       ylab = "Conductivity (µS/cm)", xlab = "Year")
bxp_llg_month_sign

stat.test <- LLG_Sites  %>%
  sign_test(Cond ~ Month) %>%
  add_significance()
stat.test
# 









