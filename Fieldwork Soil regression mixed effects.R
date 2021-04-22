# Load packages and data

library(dplyr)
library(ggplot2)
library(stargazer)

Soil <- read.csv("Soil Lab 2.csv")

#make the variables into factors so they will be recognized in the linear model
Soil$Site <- as.factor(Soil$Site)
Soil$Round <- as.factor(Soil$Round)
Soil$Side <- as.factor(Soil$Side)
Soil$Distance <- as.factor(Soil$Distance)
Soil$unprotected <- as.factor(Soil$unprotected)

hist(Soil$logSC)
#histogram looks more normal than before

#fixed effects model
logSCmodel <- lm(logSC ~ unprotected + Round + Side + Distance, data = Soil)
summary(logSCmodel)
plot(logSCmodel)

logSC.resid <- resid(logSCmodel)          
shapiro.test(logSC.resid)   
#not significant means distribution is not significantly different from normal


#mixed effects model, with site as a random effect
library(lme4)
mixed.logSC <- lmer(logSC ~ unprotected + Round + Side + Distance + (1|Site), data = Soil)
summary(mixed.logSC)

plot(mixed.logSC)

qqnorm(resid(mixed.logSC))
qqline(resid(mixed.logSC))  

stargazer(mixed.logSC, type="text", report=('vc*p'))
