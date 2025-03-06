rm(list=ls())
#--------------------------------------
# load libraries

# reading/writing data
library(readr) 
library(writexl)
library(openxlsx)
library(base)
library(stats)
library(cowplot)

# reading/writing data
library(readr) 
library(writexl)

library(phonR)
library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(lmtest)
library(tuneR)
library(seewave)
library(simr)
library(lme4)
library(readxl)

SIMR <- read_xlsx("E:/UniKonstanz/SIMR.xlsx")
#SIMR2p <- read_xlsx("E:/UniKonstanz/SIMR_2participants.xlsx")

SIMR_suitability <- SIMR %>%
  filter(test_type %in% c("suitability_scores"))

SIMR_charisma <- SIMR %>%
  filter(test_type %in% c("charisma_scores"))

SIMR_suitability %>%
  group_by(Pitch) %>%
  summarise(mean_response = mean(Response, na.rm = TRUE))

SIMR_charisma %>%
  group_by(Pitch) %>%
  summarise(mean_response = mean(Response, na.rm = TRUE))


# Fit the mixed-effects model
model <- lmer(Response ~ Pitch + (1|Participant), data=SIMR_suitability)
#model <- lmer(Response ~ Pitch * Gender + (1 | Participant), data = SIMR_suitability)
#model <- lmer(Response ~ Pitch * Gender + stereotype_scores * Pitch + 
                #political_orientation * Pitch + (1 | Participant), SIMR_suitability)

summary(model)
fixef(model)["PitchL"]

# if I expect a difference of 0.5 in the rating, with low pitch voice avatars being rated 0.5 more than higher voice avatars
fixef(model)["PitchL"] <- 0.5

# fixef(model=["PitchL"]) <- 1


powerSim(model)

# |Power for predictor 'Pitch', (95% confidence interval):==============================================|
# 77.30% (74.58, 79.86)

###############
model2 <- extend(model, along="Participant", n=80) # if I have 80 participants
powerSim(model2)

#Power for predictor 'Pitch', (95% confidence interval):===============================================================================|
# 100.0% (99.63, 100.0)


#########################
# look for interaction between pitch and gender of the avatars
model1 <- lmer(Response ~ Pitch*Gender + (1|Participant), data=SIMR_suitability)

# aggiungi fixed effect for gender and pitch as simple effect
fixef(model1)["PitchL:Gendermale"] <- 0.5 #if gender and pitch interaction effect is 0.5
fixed(model1)["PitchL"] <- 0.5

powerSim(model1, fixed("PitchL:Gendermale", method="z"))

# 28.60% (25.82, 31.51)

model2 <- extend(model1, along="Participant", n=100) # if I have 100 participants
powerSim(model2, fixed("PitchL", method="z"))

fixef(model2)["Gendermale"] <- 1
fixef(model2)["PitchL:Gendermale"] <- 1
fixed(model1)["PitchL"] <- 0.5

#Power for predictor 'PitchL:Gendermale', (95% confidence interval):=============================================================================|
# 79.60% (76.97, 82.06)

########################################################
# full model

model3 <- lmer(Response ~ Pitch*Gender + stereotype_scores*Pitch + political_orientation*Pitch + 
             stereotype_scores*Gender + political_orientation*Gender + (1|Participant), data=SIMR_suitability)
summary(model3)

fixef(model3)["PitchL"] <- 0.5
fixef(model3)["Gendermale"] <- 1
fixef(model3)["PitchL:Gendermale"] <- 1
fixef(model3)["stereotype_scores"] <- 40

fixef(model3)["PitchL"] <- 0.3 # with a smaller effect size, the observed differences between groups (or conditions) become harder to distinguish from random noise in the data.

fixef(model3)["PitchL:Gendermale"] <- 0.3

powerSim(model3, fixed("PitchL", method="z"))
# if effect size is 0.5, 13.80% (11.72, 16.09)
# if effect size is 0.3, 7.30% ( 5.77,  9.09) makes sense then that is smaller when the effect size is lower

powerSim(model3, fixed("PitchL:Gendermale", method="z"))
# 25.20% (22.54, 28.01) power decreases when introducing more IVs? could make sense

powerSim(model3, fixed("stereotype_scores"))

model4 <- extend(model3, along="Participant", n=270) # increase sample size

powerSim(model4, fixed("PitchL", method="z"))
# if effect size is 0.5, 85.00% (82.63, 87.16)
# if effect size is 0.3, 44.90% (41.79, 48.04)

powerSim(model4, fixed("PitchL:Gendermale", method="z"))
# if effect size is 0.3, 99.40% (98.70, 99.78)

powerSim(model4, fixed("stereotype_scores"))


########################################################
# charisma

SIMR_charisma <- SIMR %>%
  filter(test_type %in% c("charisma_scores"))

SIMR_charisma %>%
  group_by(Pitch) %>%
  summarise(mean_response = mean(Response, na.rm = TRUE))


model5 <- lmer(Response ~ Pitch*Gender + stereotype_scores*Pitch + political_orientation*Pitch + 
                 stereotype_scores*Gender + political_orientation*Gender + (1|Participant), data=SIMR_charisma)

fixef(model5)["PitchL"] <- 0.5
fixef(model5)["Gendermale"] <- 1
fixef(model5)["PitchL:Gendermale"] <- 1

powerSim(model5, fixed("PitchL", method="z"))

model6 <- extend(model5, along="Participant", n=300)
powerSim(model6, fixed("PitchL", method="z"))
