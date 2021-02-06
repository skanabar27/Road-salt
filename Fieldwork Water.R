# Data analysis for water data collected during fieldwork

library(dplyr)
library(ggplot2)
library(plotly)
library(plyr)
library(stars)
library(tidyr)

Water <- read.csv("Water.csv")

# just conductivity
Water_C <- Water %>%
  select(Site, C, Proximity)

# close proximity
Water_close <- Water %>%
  filter(Proximity == "Close")

# far proximity
Water_far <- Water %>%
  filter(Proximity == "Far")

# plot showing conductivity of each lake
png("fld water cond.png", units="mm", width=147, height=100, res=300)
ggplot(data = Water, aes(x = Site, y = C, fill = Proximity)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Lake",
       y = "Conductivity (µS/cm)\n",
       title = "Conductivity in HRM lakes by proximity to road salt application") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
dev.off()

# statistical tests
# conductivity across fieldwork sites
# identify outliers
Water_C %>% identify_outliers(C)
# no outliers

# check for Normality
Water_C %>% shapiro_test(C)
# p=0.0162, cannot assume normality

# median and IQR (for Wilcoxon test)
Water_C %>% get_summary_stats(C, type = "median_iqr")
# n=10, median=237, iqr=442

bxp_water <- ggboxplot(
  Water_C$C, width = 0.5, add = c("mean", "jitter"), 
  ylab = "Conductivity (µS/cm)", xlab = FALSE
)
bxp_water

gghistogram(Water_C, x = "C", y = "..density..", 
            fill = "steelblue",bins = 30, add_density = TRUE)
# distribution is somewhat symmetrical, so will continue with Wilcoxon test

stat.test <- Water_C %>%
  wilcox_test()
# there is a significant difference (p<0.001) between conductivity in 1980, 1991, and 2000









# try with SPC
ggplot(data = Water, aes(x = Site, y = SPC, fill = Proximity)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Lake",
       y = "Specific Conductivity (µS/cm)\n",
       title = "Specific conductivity in HRM lakes by proximity to road salt application") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_fill_grey(start = 0.25, end = 0.75)
#same same, just change in y-axis


# TDS
ggplot(data = Water, aes(x = Site, y = TDS, fill = Proximity)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Lake",
       y = "TDS (mg/L)\n",
       title = "Total Dissolved Solids in HRM lakes by proximity to road salt application") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_fill_grey(start = 0.25, end = 0.75)


# SAL
ggplot(data = Water, aes(x = Site, y = SAL, fill = Proximity)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Lake",
       y = "SAL (ppt)\n",
       title = "Salinity in HRM lakes by proximity to road salt application") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_fill_grey(start = 0.25, end = 0.75)


# pH
ggplot(data = Water, aes(x = Site, y = pH, fill = Proximity)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Lake",
       y = "pH\n",
       title = "pH of HRM lakes by proximity to road salt application") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.95, 0.9)) +
  scale_fill_grey(start = 0.25, end = 0.75)
# tends to be lower pH on far side


