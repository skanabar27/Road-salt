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
  select(Site, C, Proximity, unprotected)

# close proximity
Water_close <- Water %>%
  filter(Proximity == "Close")

# far proximity
Water_far <- Water %>%
  filter(Proximity == "Far")

# plot showing conductivity of each lake
png("fld water cond.png", units="mm", width=147, height=100, res=300)
ggplot(data = Water, aes(x = Site, y = C, group = Proximity, color = Proximity)) +
  geom_point(aes(shape = Proximity), na.rm = TRUE, position = position_dodge(width = 0.9)) +
  theme_classic() +
  labs(x = "\n Lake",
       y = "Water Conductivity (µS/cm)\n") +
  scale_x_discrete(limits = c("Black", "Spectacle", "Oathill", "Topsail", "Lemont")) +
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
# statistic=0.804, p=0.0162, cannot assume normality

# median and IQR (for Wilcoxon test)
Water_C %>% get_summary_stats(C, type = "median_iqr")
# n=10, median=237, iqr=442

png("fld water cond boxplot.png", units="mm", width=147, height=100, res=300)
bxp_water <- ggboxplot(
  Water_C$C, width = 0.5, add = c("mean", "jitter"), 
  ylab = "Conductivity (µS/cm)", xlab = FALSE
)
bxp_water
dev.off()

gghistogram(Water_C, x = "C", y = "..density..", 
            fill = "steelblue",bins = 30, add_density = TRUE)
# distribution is somewhat symmetrical, so will continue with Wilcoxon test

stat.water.prox <- Water_C %>%
  wilcox_test(C ~ Proximity) %>%
  add_significance()
stat.water.prox
# there is not a significant difference (statistic=11, p=0.834) between conductivity and proximity

stat.water.site <- Water_C %>%
  wilcox_test(C ~ Site) %>%
  add_significance()
stat.water.site
# there is not a significant difference between conductivity and site
# B/L, B/O, B/S, B/T, L/O, L/S, O/T p=0.333, p.adj=1
# S/T p=0.414
# L/T, O/S p=1

hist(Water$C)

# protected vs not <- prediction based on this
# Bonferroni correction (2 tests)

stat.water.protect <- Water_C %>%
  wilcox_test(C ~ unprotected) %>%
  add_significance()
stat.water.protect
# statistic=0.5, p=0.0187



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


