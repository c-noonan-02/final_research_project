# clear environment
rm(list=ls())

# load required packages
# library(dplyr)
# library(tidyr)
library(readxl)
library(writexl)
# library(hms)
# library(ggplot2)
# library(cowplot)
# library(MASS)
library(lme4)
# library(lmerTest)
# library(performance)

# import audiomoth data sets
EB_data <- read_xlsx("./audiomoth_data/EB2025_BirdNETOutput.xlsx")
head(EB_data)
BD_data <- read_xlsx("./audiomoth_data/BD2025_BirdNETOutput.xlsx")
head(BD_data)
# import bird survey data
EB_survey_data <- read_xlsx("./birdsurvey_data/EB2025_BirdSurvey.xlsx")
head(EB_survey_data)
BD_survey_data <- read_xlsx("./birdsurvey_data/BD2025_BirdSurvey.xlsx")
head(BD_survey_data)

# combine into one dataset



#### Data Arrangement ####


#### Binomial GLMMs ####

##### Model 1: No Interactions #####
phase2_mod1 <- glmer(presence~survey_method+habitat+(1|species)+(1|site)+(1|date), data = phase2_data, family = binomial)
# check model functionality
summary(phase2_mod1)
r2(phase2_mod1)
# check distribution using histogram
hist(residuals(phase2_mod1))
# check assumptions for glmer model
check_model(phase2_mod1)

##### Model 2: Interaction between method and species #####
phase2_mod2 <- glmer(presence~survey_method+habitat+(1|species)+(1|site)+(1|date)+survey_method:species, data = phase2_data, family = binomial)
# check model functionality
summary(phase2_mod2)
r2(phase2_mod2)
# check distribution using histogram
hist(residuals(phase2_mod2))
# check assumptions for glmer model
check_model(phase2_mod2)

##### Model 3: Interaction between method and habitat #####
phase2_mod3 <- glmer(presence~survey_method+habitat+(1|species)+(1|site)+(1|date)+survey_method:habitat, data = phase2_data, family = binomial)
# check model functionality
summary(phase2_mod3)
r2(phase2_mod3)
# check distribution using histogram
hist(residuals(phase2_mod3))
# check assumptions for glmer model
check_model(phase2_mod3)

##### Model 4: Interaction between method, species and habitat #####
phase2_mod4 <- glmer(presence~survey_method+habitat+(1|species)+(1|site)+(1|date)+survey_method:species:habitat, data = phase2_data, family = binomial)
# check model functionality
summary(phase2_mod4)
r2(phase2_mod4)
# check distribution using histogram
hist(residuals(phase2_mod4))
# check assumptions for glmer model
check_model(phase2_mod4)
