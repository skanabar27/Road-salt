# Data analysis for soil data

library(dplyr)
library(ggplot2)
library(plotly)
library(plyr)
library(stars)
library(tidyr)

Soil <- read.csv("Soil Lab.csv")



# try graphing - separately by lake
# Black Lake
Soil_Black <- Soil %>%
  filter(Site == "Black")

# pre-salting
Soil_Black_1 <- Soil_Black %>%
  filter(Round == "Before")

# post-salting
Soil_Black_2 <- Soil_Black %>%
  filter(Round == "After")

# all
ggplot(data = Soil_Black, aes(x = Distance, y = Soil.C, fill = Round)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (µS/cm)\n",
       title = "Soil conductivity by Black Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# round 1
ggplot(data = Soil_Black_1, aes(x = Distance, y = Soil.C, fill = Side)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (µS/cm)\n",
       title = "Soil conductivity by Black Lake pre-road salting") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# round 2
ggplot(data = Soil_Black_2, aes(x = Distance, y = Soil.C, fill = Side)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (µS/cm)\n",
       title = "Soil conductivity by Black Lake post-road salting") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))



# Oathill Lake
Soil_Oathill <- Soil %>%
  filter(Site == "Oathill")

# pre-salting
Soil_Oathill_1 <- Soil_Oathill %>%
  filter(Round == "Before")

# post-salting
Soil_Oathill_2 <- Soil_Oathill %>%
  filter(Round == "After")

# all
ggplot(data = Soil_Oathill, aes(x = Distance, y = Soil.C, fill = Round)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (µS/cm)\n",
       title = "Soil conductivity by Oathill Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# round 1
ggplot(data = Soil_Oathill_1, aes(x = Distance, y = Soil.C, fill = Side)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (µS/cm)\n",
       title = "Soil conductivity by Oathill Lake pre-road salting") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# round 2
ggplot(data = Soil_Oathill_2, aes(x = Distance, y = Soil.C, fill = Side)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (µS/cm)\n",
       title = "Soil conductivity by Oathill Lake post-road salting") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))



# Spectacle Lake
Soil_Spectacle <- Soil %>%
  filter(Site == "Spectacle")

# pre-salting
Soil_Spectacle_1 <- Soil_Spectacle %>%
  filter(Round == "Before")

# post-salting
Soil_Spectacle_2 <- Soil_Spectacle %>%
  filter(Round == "After")

# all
ggplot(data = Soil_Spectacle, aes(x = Distance, y = Soil.C, fill = Round)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (µS/cm)\n",
       title = "Soil conductivity by Spectacle Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# round 1
ggplot(data = Soil_Spectacle_1, aes(x = Distance, y = Soil.C, fill = Side)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (µS/cm)\n",
       title = "Soil conductivity by Spectacle Lake pre-road salting") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# round 2
ggplot(data = Soil_Spectacle_2, aes(x = Distance, y = Soil.C, fill = Side)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (µS/cm)\n",
       title = "Soil conductivity by Spectacle Lake post-road salting") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))



# Lemont Lake
Soil_Lemont <- Soil %>%
  filter(Site == "Lemont")

# pre-salting
Soil_Lemont_1 <- Soil_Lemont %>%
  filter(Round == "Before")

# post-salting
Soil_Lemont_2 <- Soil_Lemont %>%
  filter(Round == "After")

# all
ggplot(data = Soil_Lemont, aes(x = Distance, y = Soil.C, fill = Round)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (µS/cm)\n",
       title = "Soil conductivity by Lemont Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.9)) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# round 1
ggplot(data = Soil_Lemont_1, aes(x = Distance, y = Soil.C, fill = Side)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (µS/cm)\n",
       title = "Soil conductivity by Lemont Lake pre-road salting") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.9)) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# round 2
ggplot(data = Soil_Lemont_2, aes(x = Distance, y = Soil.C, fill = Side)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (µS/cm)\n",
       title = "Soil conductivity by Lemont Lake post-road salting") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.7)) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))



# Topsail Lake
Soil_Topsail <- Soil %>%
  filter(Site == "Topsail")

# pre-salting
Soil_Topsail_1 <- Soil_Topsail %>%
  filter(Round == "Before")

# post-salting
Soil_Topsail_2 <- Soil_Topsail %>%
  filter(Round == "After")

# all
ggplot(data = Soil_Topsail, aes(x = Distance, y = Soil.C, fill = Round)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (µS/cm)\n",
       title = "Soil conductivity by Topsail Lake") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.9)) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# round 1
ggplot(data = Soil_Topsail_1, aes(x = Distance, y = Soil.C, fill = Side)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (µS/cm)\n",
       title = "Soil conductivity by Topsail Lake pre-salt rounding") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.9, 0.9)) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# round 2
ggplot(data = Soil_Topsail_2, aes(x = Distance, y = Soil.C, fill = Side)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  labs(x = "\n Distance (m)",
       y = "Conductivity (µS/cm)\n",
       title = "Soil conductivity by Topsail Lake post-road salting") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10),
        legend.position = c(0.7, 0.8)) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))



# all all
Soil$Round <- factor(Soil$Round, levels = c("Before", "After"))
Soil$Side <- factor(Soil$Side, levels = c("Close", "Far"))

png("soil cond.png", units="mm", width=147, height=100, res=300)
ggplot(data = Soil, aes(x = Distance, y = Soil.C, fill = Round)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  theme_classic() +
  facet_wrap(vars(Site, Side), nrow = 2) + 
  labs(x = "\n Distance (m)",
       y = "Conductivity (µS/cm)\n",
       fill = NULL) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10)) +
  scale_fill_grey(start = 0.25, end = 0.75) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))
dev.off()



# statistical tests
# conductivity across fieldwork sites
# identify outliers
Soil %>% identify_outliers(Soil.C)
# Black Before Far 0 / Oathill Before Far 0 non-extreme outliers
# Spectacle Before Close 0 is an extreme outlier

# check for Normality
Soil %>% shapiro_test(Soil.C)
# statistic=0.850, p<0.001, cannot assume normality

# median and IQR (for Wilcoxon test)
Soil %>% get_summary_stats(Soil.C, type = "median_iqr")
# n=60, median=99.1, iqr=82.5

png("soil cond boxplot.png", units="mm", width=147, height=100, res=300)
bxp_soil <- ggboxplot(
  Soil$Soil.C, width = 0.5, add = c("mean", "jitter"), 
  ylab = "Conductivity (µS/cm)", xlab = FALSE
)
bxp_soil
dev.off()

gghistogram(Soil, x = "Soil.C", y = "..density..", 
            fill = "steelblue",bins = 30, add_density = TRUE)
# distribution is somewhat symmetrical, so will continue with Wilcoxon test

stat.soil.site <- Soil %>%
  wilcox_test(Soil.C ~ Site) %>%
  add_significance()
stat.soil.site
# B/L, L/S, L/T p<0.001
# B/O, B/S, B/T, L/O, O/S, O/T, S/T p>0.05

stat.soil.distance <- Soil %>%
  wilcox_test(Soil.C ~ Distance) %>%
  add_significance()
stat.soil.distance
# there is not a significant difference between conductivity and distance
# 0/10 p=0.529, 0/20 p=0.192, 10/20 p=0.341

stat.soil.round <- Soil %>%
  wilcox_test(Soil.C ~ Round) %>%
  add_significance()
stat.soil.round
# there is not a significant difference between conductivity and round (p=0.123)

stat.soil.side <- Soil %>%
  wilcox_test(Soil.C ~ Side) %>%
  add_significance()
stat.soil.side
# there is not a significant difference between conductivity and distance (p=0.0551)



Soil2 <- read.csv("Soil Lab 2.csv")
mice2 <- Soil %>%
  group_by(Round) %>%
  get_summary_stats(Soil.C, type = "mean_sd")

bxp <- ggpaired(mice2, x = "Round", y = "variable", 
                order = c("Before", "After"),
                ylab = "Conductivity (µS/cm)", xlab = "Round")
bxp         #not right

mice2 <- mice2 %>% mutate(differences = before - after)
head(mice2, 3)



# sieve bootstrap version of the t-test
# no assumption of Normality / independence 
notrend_test(Soil$Soil.C)
# t-value=0.67703, p=0.345
# phi_1=-0.1953815, phi_2=-0.2744110, phi_3=0.2722356
# there is no linear trend

  