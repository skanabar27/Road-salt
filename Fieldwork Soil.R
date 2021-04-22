# Data analysis for soil data

library(dplyr)
library(ggplot2)
library(plotly)
library(plyr)
library(stars)
library(tidyr)


# scatterplot of all samples
Soil_2 <- read.csv("Soil Lab 2.csv")
Soil_2$unprotected <- factor(Soil_2$unprotected, levels = c(1, 0), labels = c("Exposed", "Protected"))
Soil_2$Round <- factor(Soil_2$Round, levels = c("Before", "After"))

png("soil cond all.png", units="mm", width=147, height=100, res=300)
ggplot(data = Soil_2, aes(x = Site, y = Soil.C, color = Round)) +
  geom_point(aes(shape = Side), na.rm = TRUE, position = position_dodge(width = 0.9)) +
  theme_classic() +
  labs(x = "\n Lake",
       y = "Soil Conductivity (µS/cm)\n") +
  facet_wrap(~ Distance, labeller = labeller(Distance = 
                                              c("0" = "0 m",
                                                "10" = "10 m",
                                                "20" = "20 m"))) +
  scale_x_discrete(limits = c("Black", "Spectacle", "Oathill", "Topsail", "Lemont")) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 10)) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        axis.text.x = element_text(size = 10, angle = 90),
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

# switch to "Fieldwork Soil regression mixed effects.R"

