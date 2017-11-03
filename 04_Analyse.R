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
stroke.binom <- svyglm(CVDSTRK3 ~ RACE.ETH + AGE.GROUP + INCOME2 + SEX + 
                                     EDUCA + MARITAL + N.CHILDREN + EXERANY2 +
                                     ADDEPEV2 + HEAVY.DRINKER + SMOKER.STATUS + MARIJANA.5GRP +
                                     STATE,
                                   brfss.survey.design, family = "binomial")

# phi is nearly 1.5 - lots of dispersion. Should use quasibinomial
phi.b <- sum(resid(satisfied.surveylogistic.b, type = "pearson")^2) / df.residual(satisfied.surveylogistic.b)

satisfied.surveylogistic.qb <- svyglm(CVDSTRK3 ~ RACE.ETH + AGE.GROUP + INCOME2 + SEX + 
                                     EDUCA + MARITAL + N.CHILDREN + EXERANY2 +
                                     ADDEPEV2 + HEAVY.DRINKER + SMOKER.STATUS + MARIJANA.5GRP +
                                     STATE,
                                   brfss.survey.design, family = "quasibinomial")

# Tables ----
results$table.exercise.sex <- table(ourData$sex, ourData$any.exercise)
results$table.mj.sex <- table(ourData$sex, ourData$use.mj)
results$table.mj.satisfied <- table(ourData$satisfied, ourData$use.mj)
results$table.exercise.satisfied <- table(ourData$satisfied, ourData$any.exercise)

# Plots ----
results$bar_sex_income <- ggplot(subset(ourData, !is.na(sex)), aes(x=factor(sex), y=income)) +
  stat_summary(fun.y="median", geom="bar")

results$bar_sleep_hrs_sex <- ggplot(subset(ourData, !is.na(sex)), aes(x = factor(sex), y = sleep.hrs)) +
  stat_summary(fun.y = "mean", geom="bar")

results$bar_exercise_hours_sex <- ggplot(subset(ourData, !is.na(sex)), aes(x = factor(sex), y = any.exercise)) +
  stat_summary(fun.y = "median", geom ="bar")

# Save the results object ----
saveRDS(results, file = "Data/results.rds")
