# Data analysis for synoptic lakes dataset

library(dplyr)
library(ggplot2)
library(plotly)
library(plyr)
#library(sf)
library(stars)
library(tidyr)

# add condensed dataset with salt and conductivity data
# sf library is corrupt?
lake <- read.csv("Synoptic salt.csv")


