#library(tidyverse)
# library(broom)
# library(modelr)
library(ggplot2)

#Disable scientific notation
options(scipen=999)

# Initialize an empty list to store the results ----
results <- list()

# Load the transformed data ----
readRDS("Data/transformed.rds") -> brfss.survey.design

# Specific transformations ----

# Models ----
# First one here is standard binomial model for surveys
stroke.binom <- svyglm(CVDSTRK3 ~ RACE.ETH + AGE.GROUP + INCOME2 + SEX + 
                                     EDUCA + MARITAL + N.CHILDREN + EXERANY2 +
                                     ADDEPEV2 + HEAVY.DRINKER + SMOKER.STATUS + MARIJANA.5GRP +
                                     STATE,
                                   brfss.survey.design, family = "binomial")

# Check phi to determine if we need to use the quasibinomial method (adds a dispersion paramter)
phi.b <- sum(resid(satisfied.surveylogistic.b, type = "pearson")^2) / df.residual(satisfied.surveylogistic.b)

# Quasibinomial method to use if phi is too large
satisfied.surveylogistic.qb <- svyglm(CVDSTRK3 ~ RACE.ETH + AGE.GROUP + INCOME2 + SEX + 
                                     EDUCA + MARITAL + N.CHILDREN + EXERANY2 +
                                     ADDEPEV2 + HEAVY.DRINKER + SMOKER.STATUS + MARIJANA.5GRP +
                                     STATE,
                                   brfss.survey.design, family = "quasibinomial")

# Tables ----

# Plots ----

# Save the results object ----
saveRDS(results, file = "Data/results.rds")
