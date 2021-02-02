# Data analysis for Lemont 

library(dplyr)
library(ggplot2)
library(plotly)
library(plyr)
library(stars)
library(tidyr)

LLDL01 <- read.csv("LLDL-01.csv")

# conductivity
LL_cond <- LLDL01 %>%
  filter("")
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
ggplot(LLG_cond_year) +
  geom_line(aes(x = Year, y = Mean_Conductivity)) +
  theme_classic() +
  labs(x = "\n Year",
       y = "Mean Conductivity (µS/cm)\n",
       title = "Mean Conductivity in Lemont Lake over time") +
  theme(plot.title = element_text(hjust = 0.5))

# with sd
ggplot(LLG_cond_year) +
  geom_line(aes(x = Year, y = Mean_Conductivity)) +
  theme_classic() +
  labs(x = "\n Year",
       y = "Mean Conductivity (µS/cm)\n",
       title = "Mean Conductivity in Lemont Lake over time") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(mapping = aes(x = Year,
                              ymin = Mean_Conductivity-SD_Conductivity,
                              ymax = Mean_Conductivity+SD_Conductivity), width = 0.2)


# conductivity by month
LLG_cond_month <- LLG_Sites %>%
  dplyr::group_by(Month) %>%
  dplyr::summarise(Mean_Conductivity = mean(Cond), SD_Conductivity = sd(Cond)) 

                        #LLG_cond_month$Month <- as.numeric(LLG_cond_month$Month)

LLG_cond_month <- LLG_cond_month %>%
  arrange(Month = c("January", "February", "March", "April", "May", "June", "July", 
                    "August", "September", "October", "November", "December"))

LLG_cond_month <- LLG_cond_month %>%
  mutate(Month = factor(as.character(Month),
                        levels = c(1:12),
                        labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))

# count per month
LLG_cond_month_count <- LLG_Sites %>%
  dplyr::group_by(Month) %>%
  dplyr::summarise(count = length(Cond))
# ~20-32 samples, except Feb only has 9

# without sd
ggplot(LLG_cond_month) +
  geom_line(aes(x = Month, y = Mean_Conductivity, group = 1)) +
  theme_classic() +
  labs(x = "\n Month",
       y = "Mean Conductivity (µS/cm)\n",
       title = "Mean Conductivity in Lemont Lake per month") +
  theme(plot.title = element_text(hjust = 0.5))

# with sd
ggplot(LLG_cond_month) +
  geom_line(aes(x = Month, y = Mean_Conductivity, group = 1)) +
  theme_classic() +
  labs(x = "\n Month",
       y = "Mean Conductivity (µS/cm)\n",
       title = "Mean Conductivity in Lemont Lake per month") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_errorbar(mapping = aes(x = Month,
                              ymin = Mean_Conductivity-SD_Conductivity,
                              ymax = Mean_Conductivity+SD_Conductivity), width = 0.2)
  


