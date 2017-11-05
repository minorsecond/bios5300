#library(tidyverse)
# library(broom)
# library(modelr)
library(ggplot2)
library(srvyr)
library(survey)

#Disable scientific notation
options(scipen=999)

# Initialize an empty list to store the results ----
results <- list()

# Load the transformed data ----
readRDS("Data/transformed.rds") -> brfss.survey.design

# Specific transformations ----

# Models ----
# Relevel some of the factors so that the regression output is easier to interpret.
brfss.survey.design$variables$sex <- relevel(brfss.survey.design$variables$sex, ref = "Male")
brfss.survey.design$variables$smoker.status <- relevel(brfss.survey.design$variables$smoker.status, ref = "Never smoked")
brfss.survey.design$variables$n.children <- relevel(brfss.survey.design$variables$n.children, ref = "No children in household")
brfss.survey.design$variables$diag.depression <- relevel(brfss.survey.design$variables$diag.depression, ref = "No")
brfss.survey.design$variables$smoker.status <- relevel(brfss.survey.design$variables$smoker.status, ref = "Never smoked")
brfss.survey.design$variables$diff.walking <- relevel(brfss.survey.design$variables$diff.walking, ref = "No")
brfss.survey.design$variables$diff.errands.alone <- relevel(brfss.survey.design$variables$diff.errands.alone, ref = "No")
brfss.survey.design$variables$diff.walking <- relevel(brfss.survey.design$variables$diff.walking, ref = "No")
brfss.survey.design$variables$race.eth <- relevel(brfss.survey.design$variables$race.eth, ref = "Other race only, Non-Hispanic")
brfss.survey.design$variables$age.grp <- relevel(brfss.survey.design$variables$age.grp, ref = "Age 18 to 24")
brfss.survey.design$variables$exercises.yn <- relevel(brfss.survey.design$variables$exercises.yn, ref = "No")
brfss.survey.design$variables$veteran <- relevel(brfss.survey.design$variables$veteran, ref = "No")

# First one here is standard binomial model for surveys
results$stroke.binom <- svyglm(diag.stroke ~ alc.heavy.drinker + smoker.status + n.children +
                                 days.anxious + days.sad + 
                                 days.poor.mental.health + diag.depression + 
                                 daily.sleep.hrs + exercises.yn + bmi + 
                                 sex + veteran + race.eth + age.grp,
                               brfss.survey.design, family = "binomial")

# Calculate OR and 95% CI for estimate
results$stroke.binom.ORCI <- exp(cbind(OR = coef(results$stroke.binom), confint(results$stroke.binom)))

# Check phi to determine if we need to use the quasibinomial method (adds a dispersion paramter)
results$phi.b <- sum(resid(results$stroke.binom, type = "pearson")^2) / df.residual(results$stroke.binom)

# Quasibinomial method to use if phi is too large

results$stroke.qbinom <- svyglm(diag.stroke ~ alc.heavy.drinker + smoker.status + n.children +
                                  days.anxious + days.sad +  days.poor.mental.health +
                                  diag.depression + daily.sleep.hrs + exercises.yn + bmi + 
                                  sex + veteran + race.eth + age.grp,
                                brfss.survey.design, family = "quasibinomial")

# Calculate OR and 95% CI for estimate
results$stroke.qb.ORCI <- exp(cbind(OR = coef(results$stroke.qbinom), confint(results$stroke.qbinom)))

#KW test to determine if men/women have more strokes
svyranktest(CVDSTRK3 ~ SEX, design = brfss.survey.design)

# Tables ----

results$stroke.exercise <- svytable(~CVDSTRK3 + EXERCISE, design = brfss.survey.design)
colnames(results$stroke.exercise) <- c("No exercise in past 30 days", "Exercised in past 30 days")
rownames(results$stroke.exercise) <- c("No stroke diagnosis", "Stroke diagnosis")
results$stroke.exercise.p <- prop.table(results$stroke.exercise, 1)
#results$stroke.exercise.chisq <- svychisq(~CVDSTRK3 + EXERCISE, design = brfss.survey.design)

results$stroke.sex <- na.omit(svytable(~diag.stroke + sex, design = brfss.survey.design))
colnames(results$stroke.sex) <- c("Male", "Female")
rownames(results$stroke.sex) <- c("No stroke diagnosis", "Stroke diagnosis")
results$stroke.sex.p <- prop.table(results$stroke.sex, 1)

# Plots ----


# Save the results object ----
saveRDS(results, file = "Data/results.rds")
