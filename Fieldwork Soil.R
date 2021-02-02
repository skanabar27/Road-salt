# Data analysis for soil data

library(dplyr)
library(ggplot2)
library(plotly)
library(plyr)
library(stars)
library(tidyr)

Soil <- read.csv("Soil.csv")

Soil_Cond_pool <- Soil %>%
  select(Site, Round, Proximity, Distance, Avg.C, SD.C)


# try graphing - separately by lake
# Black Lake
Soil_Cond_Black <- Soil_Cond_pool %>%
  filter(Site == "Black")

# pre-salting
Soil_Cond_Black_1 <- Soil_Cond_Black %>%
  filter(Round == "1")

# post-salting
Soil_Cond_Black_2 <- Soil_Cond_Black %>%
  filter(Round == "2")

# all
ggplot(data = Soil_Cond_Black, aes(x = Distance, y = Avg.C, color = Proximity)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (mS/cm)\n",
       title = "Soil conductivity by Black Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))

# round 1
ggplot(data = Soil_Cond_Black_1, aes(x = Distance, y = Avg.C, color = Proximity)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (mS/cm)\n",
       title = "Soil conductivity by Black Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))

# round 2
ggplot(data = Soil_Cond_Black_2, aes(x = Distance, y = Avg.C, color = Proximity)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (mS/cm)\n",
       title = "Soil conductivity by Black Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))



# Oathill Lake
Soil_Cond_Oathill <- Soil_Cond_pool %>%
  filter(Site == "Oathill")

# pre-salting
Soil_Cond_Oathill_1 <- Soil_Cond_Oathill %>%
  filter(Round == "1")

# post-salting
Soil_Cond_Oathill_2 <- Soil_Cond_Oathill %>%
  filter(Round == "2")

# all
ggplot(data = Soil_Cond_Oathill, aes(x = Distance, y = Avg.C, color = Proximity)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (mS/cm)\n",
       title = "Soil conductivity by Oathill Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))

# round 1
ggplot(data = Soil_Cond_Oathill_1, aes(x = Distance, y = Avg.C, color = Proximity)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (mS/cm)\n",
       title = "Soil conductivity by Oathill Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))

# round 2
ggplot(data = Soil_Cond_Black_2, aes(x = Distance, y = Avg.C, color = Proximity)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (mS/cm)\n",
       title = "Soil conductivity by Oathill Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))



# Spectacle Lake
Soil_Cond_Spectacle <- Soil_Cond_pool %>%
  filter(Site == "Spectacle")

# pre-salting
Soil_Cond_Spectacle_1 <- Soil_Cond_Spectacle %>%
  filter(Round == "1")

# post-salting
Soil_Cond_Spectacle_2 <- Soil_Cond_Spectacle %>%
  filter(Round == "2")

# all
ggplot(data = Soil_Cond_Spectacle, aes(x = Distance, y = Avg.C, color = Proximity)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (mS/cm)\n",
       title = "Soil conductivity by Spectacle Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))

# round 1
ggplot(data = Soil_Cond_Spectacle_1, aes(x = Distance, y = Avg.C, color = Proximity)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (mS/cm)\n",
       title = "Soil conductivity by Spectacle Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))

# round 2
ggplot(data = Soil_Cond_Spectacle_2, aes(x = Distance, y = Avg.C, color = Proximity)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (mS/cm)\n",
       title = "Soil conductivity by Spectacle Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))



# Lemont Lake
Soil_Cond_Lemont <- Soil_Cond_pool %>%
  filter(Site == "Lemont")

# pre-salting
Soil_Cond_Lemont_1 <- Soil_Cond_Lemont %>%
  filter(Round == "1")

# post-salting
Soil_Cond_Lemont_2 <- Soil_Cond_Lemont %>%
  filter(Round == "2")

# all
ggplot(data = Soil_Cond_Lemont, aes(x = Distance, y = Avg.C, color = Proximity)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (mS/cm)\n",
       title = "Soil conductivity by Lemont Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))

# round 1
ggplot(data = Soil_Cond_Lemont_1, aes(x = Distance, y = Avg.C, color = Proximity)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (mS/cm)\n",
       title = "Soil conductivity by Lemont Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.4)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))

# round 2
ggplot(data = Soil_Cond_Lemont_2, aes(x = Distance, y = Avg.C, color = Proximity)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (mS/cm)\n",
       title = "Soil conductivity by Lemont Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))



# Topsail Lake
Soil_Cond_Topsail <- Soil_Cond_pool %>%
  filter(Site == "Topsail")

# pre-salting
Soil_Cond_Topsail_1 <- Soil_Cond_Topsail %>%
  filter(Round == "1")

# post-salting
Soil_Cond_Topsail_2 <- Soil_Cond_Topsail %>%
  filter(Round == "2")

# all
ggplot(data = Soil_Cond_Topsail, aes(x = Distance, y = Avg.C, color = Proximity)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (mS/cm)\n",
       title = "Soil conductivity by Topsail Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.5)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))

# round 1
ggplot(data = Soil_Cond_Topsail_1, aes(x = Distance, y = Avg.C, color = Proximity)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (mS/cm)\n",
       title = "Soil conductivity by Topsail Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))

# round 2
ggplot(data = Soil_Cond_Topsail_2, aes(x = Distance, y = Avg.C, color = Proximity)) +
  geom_point(alpha = 0.5, size = 2) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (mS/cm)\n",
       title = "Soil conductivity by Topsail Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_color_manual(values = c("mediumvioletred", "midnightblue"))






