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
                                 as.integer(DAYS.ANXIOUS) + as.integer(DAYS.ENERGETIC) +  + as.integer(DAYS.ANXIOUS) + 
                                 as.integer(MENTHLTH) + GOOD.HEALTH + ADDEPEV2 + as.integer(SLEPTIM1) + EXERCISE + 
                                 GENHLTH + RACE.ETH + HIGH.RISK + AGE.GROUP + X_BMI5 + STATE,
                               brfss.survey.design, family = "binomial")

# Calculate OR and 95% CI for estimate
results$stroke.binom.ORCI <- exp(cbind(OR = coef(results$stroke.binom), confint(results$stroke.binom)))

# Check phi to determine if we need to use the quasibinomial method (adds a dispersion paramter)
results$phi.b <- sum(resid(results$stroke.binom, type = "pearson")^2) / df.residual(results$stroke.binom)

# Quasibinomial method to use if phi is too large
results$stroke.qbinom <- svyglm(CVDSTRK3 ~ HEAVY.DRINKER + SMOKER.STATUS +
                                  as.integer(DAYS.ANXIOUS) + as.integer(DAYS.ENERGETIC) + as.integer(DAYS.ANXIOUS) + 
                                  as.integer(MENTHLTH) + ADDEPEV2 + as.integer(SLEPTIM1) + as.factor(EXERCISE) + 
                                  RACE.ETH + AGE.GROUP + X_BMI5,
                                brfss.survey.design, family = "quasibinomial")
# Calculate OR and 95% CI for estimate
results$stroke.qb.ORCI <- exp(cbind(OR = coef(results$stroke.qbinom), confint(results$stroke.qbinom)))

# Tables ----

results$stroke.exercise <- svytable(~CVDSTRK3 + EXERCISE, design = brfss.survey.design)
colnames(results$stroke.exercise) <- c("No exercise in past 30 days", "Exercised in past 30 days")
rownames(results$stroke.exercise) <- c("Stroke diagnosis", "No stroke diagnosis")
results$stroke.exercise.p <- prop.table(results$stroke.exercise, 1)
#results$stroke.exercise.chisq <- svychisq(~CVDSTRK3 + EXERCISE, design = brfss.survey.design)

# Plots ----

# Save the results object ----
saveRDS(results, file = "Data/results.rds")
