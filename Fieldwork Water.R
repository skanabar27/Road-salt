# Data analysis for water data collected during fieldwork

library(dplyr)
library(ggplot2)
library(plotly)
library(plyr)
library(stars)
library(tidyr)

Water <- read.csv("Water.csv")

# close proximity
Water_close <- Water %>%
  filter(Proximity == "close")

# far proximity
Water_far <- Water %>%
  filter(Proximity == "far")

# plot showing conductivity of each lake
ggplot(data = Water, aes(x = Site, y = C, color = Proximity)) +
  geom_bar(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Lake",
       y = "Conductivity (µS/cm)\n",
       title = "Conductivity in HRM lakes by proximity to road salt application") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))
                                                                        #use bar plot instead
# t-test for conductivity




# try with SPC
ggplot(data = Water, aes(x = Site, y = SPC, color = Proximity)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Lake",
       y = "Specific Conductivity (µS/cm)\n",
       title = "Specific conductivity in HRM lakes by proximity to road salt application") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))
#same same, just change in y-axis


# TDS
ggplot(data = Water, aes(x = Site, y = TDS, color = Proximity)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Lake",
       y = "TDS (mg/L)\n",
       title = "Total Dissolved Solids in HRM lakes by proximity to road salt application") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))


# SAL
ggplot(data = Water, aes(x = Site, y = SAL, color = Proximity)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Lake",
       y = "SAL (ppt)\n",
       title = "Salinity in HRM lakes by proximity to road salt application") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))


# pH
ggplot(data = Water, aes(x = Site, y = pH, color = Proximity)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Lake",
       y = "pH\n",
       title = "pH of HRM lakes by proximity to road salt application") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.85)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))


