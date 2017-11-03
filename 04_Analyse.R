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
results$stroke.binom <- svyglm(CVDSTRK3 ~ HEAVY.DRINKER + SMOKER.STATUS + BINGE.DRINKER +
                                 DAYS.ANXIOUS + DAYS.ENERGETIC + MENTHLTH + GOOD.HEALTH +
                                 ADDEPEV2 + SLEPTIM1 + EXERANY2 + GENHLTH + N.CHILDREN +
                                 EDUCA + INCOME2 + RACE.ETH + HIGH.RISK + DAYS.ANXIOUS +
                                 STATE,
                               brfss.survey.design, family = "binomial")

# Check phi to determine if we need to use the quasibinomial method (adds a dispersion paramter)
results$phi.b <- sum(resid(results$stroke.binom, type = "pearson")^2) / df.residual(results$stroke.binom)

# Quasibinomial method to use if phi is too large
results$satisfied.surveylogistic.qb <- svyglm(CVDSTRK3 ~ HEAVY.DRINKER + SMOKER.STATUS + BINGE.DRINKER +
                                                DAYS.ANXIOUS + DAYS.ENERGETIC + MENTHLTH + GOOD.HEALTH +
                                                ADDEPEV2 + SLEPTIM1 + EXERANY2 + GENHLTH + N.CHILDREN +
                                                EDUCA + INCOME2 + RACE.ETH + HIGH.RISK + DAYS.ANXIOUS +
                                                STATE,
                                              brfss.survey.design, family = "quasibinomial")

# Tables ----

# Plots ----

# Save the results object ----
saveRDS(results, file = "Data/results.rds")
