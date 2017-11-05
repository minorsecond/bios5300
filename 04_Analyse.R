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
brfss.survey.design$variables$diag.nonskin.cancer <- relevel(brfss.survey.design$variables$diag.nonskin.cancer, ref = "No")
brfss.survey.design$variables$current.cancer.treatment <- relevel(brfss.survey.design$variables$current.cancer.treatment, ref = "No - Havent Started")
brfss.survey.design$variables$health.care.coverage.source <- relevel(brfss.survey.design$variables$health.care.coverage.source, ref = "No Coverage")
brfss.survey.design$variables$time.since.care.coverage <- relevel(brfss.survey.design$variables$time.since.care.coverage, ref = "Never")

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

results$stroke.qbinom <- svyglm(diag.stroke ~ alc.heavy.drinker + smoker.status + 
                                  days.anxious + diag.depression + 
                                  daily.sleep.hrs + sex + income + race.eth + age.grp,
                                brfss.survey.design, family = "quasibinomial")

# Calculate OR and 95% CI for estimate
results$stroke.qb.ORCI <- exp(cbind(OR = coef(results$stroke.qbinom), confint(results$stroke.qbinom)))

#KW test to determine if men/women have more strokes
results$kwallis.stroke.by.sex <- svyranktest(diag.stroke ~ sex, design = brfss.survey.design)

# Tables ----

# Summary statistics
results$mean.sleep.by.outcome <- svyby(~daily.sleep.hrs, ~diag.stroke, brfss.survey.design, svyquantile, quantiles = c(0.5), keep.var = F, na.rm = T)
results$mean.bmi.by.outcome <- svyby(~bmi, ~diag.stroke, brfss.survey.design, svyquantile, quantiles = c(0.5), keep.var = F, na.rm = T)
results$mean.days.anxious.by.outcome <- svyby(~days.anxious, ~diag.stroke, brfss.survey.design, svyquantile, quantiles = c(0.5), keep.var = F, na.rm = T)
results$mean.n.drinks.weekly.by.outcome <- svyby(~alc.n.drinks.weekly, ~diag.stroke, brfss.survey.design, svyquantile, quantiles = c(0.5), keep.var = F, na.rm = T)


# Two-way tables
results$stroke.exercise <- svytable(~CVDSTRK3 + EXERCISE, design = brfss.survey.design)
colnames(results$stroke.exercise) <- c("No exercise in past 30 days", "Exercised in past 30 days")
rownames(results$stroke.exercise) <- c("No stroke diagnosis", "Stroke diagnosis")
results$stroke.exercise.p <- prop.table(results$stroke.exercise, 1)
#results$stroke.exercise.chisq <- svychisq(~CVDSTRK3 + EXERCISE, design = brfss.survey.design)

results$stroke.sex <- na.omit(svytable(~diag.stroke + sex, design = brfss.survey.design))
colnames(results$stroke.sex) <- c("Male", "Female")
rownames(results$stroke.sex) <- c("No stroke diagnosis", "Stroke diagnosis")
results$stroke.sex.p <- prop.table(results$stroke.sex, 1)

results$stroke.heavy.drinker <- na.omit(svytable(~diag.stroke + alc.heavy.drinker, design = brfss.survey.design))
colnames(results$stroke.heavy.drinker) <- c("Non-heavy Drinker", "Heavy Drinker")
rownames(results$stroke.heavy.drinker) <- c("No stroke diagnosis", "Stroke diagnosis")
results$stroke.heavy.drinker.p <- prop.table(results$stroke.heavy.drinker, 1)

results$stroke.veteran <- na.omit(svytable(~diag.stroke + veteran, design = brfss.survey.design))
colnames(results$stroke.veteran) <- c("Non-Veteran", "Veteran")
rownames(results$stroke.veteran) <- c("No stroke diagnosis", "Stroke diagnosis")
results$stroke.veteran.p <- prop.table(results$stroke.veteran, 1)

# Plots ----
results$box.sleep <- svyboxplot(daily.sleep.hrs ~ diag.stroke, design = brfss.survey.design)
results$box.anxious <- svyboxplot(days.anxious ~ diag.stroke, design = brfss.survey.design)
results$box.bmi <- svyboxplot(bmi ~ diag.stroke, design = brfss.survey.design)
results$box.ndrinks.weekly <- svyboxplot(alc.n.drinks.weekly ~ diag.stroke, design = brfss.survey.design)

# Save the results object ----
saveRDS(results, file = "Data/results.rds")
