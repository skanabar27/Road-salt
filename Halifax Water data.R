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
library(funtimes)
library(lubridate)
library(forecast)
library(colortools)

                                                
LLG_Sites <- read.csv("LLG_Sites.csv")

png("hw cond.png", units="mm", width=147, height=100, res=300)
ggplot(LLG_Sites) +
  geom_point(aes(x = Year, y = Cond), alpha = 0.4) +
  theme_classic() +
  labs(x = "\n Year",
       y = "Conductivity (µS/cm)\n") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10)) +
  scale_x_continuous(breaks = seq(2010, 2018, by = 4)) +
  stat_summary(aes(x = Year, y = Cond), fun = "mean", geom = "line")
dev.off()

# count per year
LLG_cond_year_count <- LLG_Sites %>%
  dplyr::group_by(Year) %>%
  dplyr::summarise(count = length(Cond))
# 2009:9, 2010:31, 2011:32, 2012:36, 2013:28, 2014:18, 2015:21, 2016:15, 2017:30, 2018:34, 2019:17, 2020:4


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


# timeseries
LLG_Sites <- read.csv("LLG_Sites.csv")

class(LLG_Sites$Date)
LLG_Sites$Date <- as.Date(LLG_Sites$Date, format = "%d-%b-%y")
LLG_Sites$Sample.Site.ID <- factor(LLG_Sites$Sample.Site.ID, labels = c(1, 2, 3))
LLG_Sites$Sample.Site.ID <- rename(LLG_Sites$Sample.Site.ID, labels = c(Site))

LLG_01 <- LLG_Sites %>%
  select(Sample.Site.ID, Date, Cond) %>%
  filter(Sample.Site.ID == "LLG-01")
LLG_02 <- LLG_Sites %>%
  select(Sample.Site.ID, Date, Cond) %>%
  filter(Sample.Site.ID == "LLG-02")
LLG_03 <- LLG_Sites %>%
  select(Sample.Site.ID, Date, Cond) %>%
  filter(Sample.Site.ID == "LLG-03")

png("hw time cond separate.png", units="mm", width=147, height=100, res=300)
time_plot <- ggplot(LLG_Sites, aes(x = Date, y = Cond, colour = Sample.Site.ID)) +
    geom_line() +
    scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
    theme_classic() +
  labs(x = "\n Year",
       y = "Conductivity (µS/cm)\n") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.9, 0.8)) +
  scale_colour_discrete("Sample Site")
time_plot
dev.off()


# conductivity by date
LLG_cond_date <- LLG_Sites %>%
  dplyr::group_by(Date) %>%
  dplyr::summarise(Mean_Conductivity = mean(Cond), SD_Conductivity = sd(Cond))

png("hw time cond mean.png", units="mm", width=147, height=100, res=300)
time_plot_mean <- ggplot(LLG_cond_date, aes(x = Date, y = Mean_Conductivity)) +
  geom_line() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_classic() +
  labs(x = "\n Year",
       y = "Conductivity (µS/cm)\n") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.9, 0.8))
time_plot_mean
dev.off()


# all conductivity by date
class(LLG_cond_all$Date)
LLG_cond_all$Date <- as.Date(LLG_cond_all$Date, format = "%d-%b-%y")

png("hw time cond all.png", units="mm", width=147, height=100, res=300)
time_plot_mean_a <- ggplot(LLG_cond_all, aes(x = Date, y = Cond)) +
  geom_line() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  theme_classic() +
  labs(x = "\n Year",
       y = "Conductivity (µS/cm)\n") +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.9, 0.8))
time_plot_mean_a
dev.off()


# Extract month and year and store in separate columns
LLG_cond_all$Year <- format(LLG_cond_all$Date, format = "%Y")
LLG_cond_all$Month <- format(LLG_cond_all$Date, format = "%b")
LLG_cond_all$Month <- factor(LLG_cond_all$Month, levels = month.abb)

# Create a colour palette using the `colortools` package 
year_pal <- sequential(color = "darkturquoise", percentage = 5, what = "value")
display.brewer.all()
display.brewer.pal(12, "Set3")

# Make the plot
png("hw time cond year.png", units="mm", width=147, height=100, res=300)
(seasonal <- ggplot(LLG_cond_date, aes(x = Month, y = Mean_Conductivity, group = Year)) +
    geom_line(aes(colour = Year)) +
    theme_classic() + 
    scale_color_brewer(palette = "Set3")) +
  labs(x = "\n Year",
       y = "Conductivity (µS/cm)\n") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


# statistical tests not completed as variables do not have the same length
