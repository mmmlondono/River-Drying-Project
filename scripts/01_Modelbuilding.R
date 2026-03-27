####READ ME####
#The purpose of this script is to test linear and non linear regression models to predict
#drying in the Rio Grande

####Libraries ####
library(tidyverse)
library(DHARMa)
library(lme4) # for creating mixed models
library(car) # for Anova(), vif()
library(MuMIn) # for AICc
library(emmeans) # for emmeans, emtrends, all the post hoc tests and plotting

####Linear models####
# create the linear model
m1 <- lm(discharge$X_00060_00003 ~ Annual_dry_rm$Sum_days_rm_dry, na.action=na.omit)
# check assumptions
plot(mass_species_m1)