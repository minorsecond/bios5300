#library(tidyverse)
# library(broom)
# library(modelr)
library(ggplot2)
library(srvyr)
library(survey)
library(RColorBrewer)

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
#brfss.survey.design$variables$n.children <- relevel(brfss.survey.design$variables$n.children, ref = "No children in household")
brfss.survey.design$variables$diag.depression <- relevel(brfss.survey.design$variables$diag.depression, ref = "No")
brfss.survey.design$variables$smoker.status <- relevel(brfss.survey.design$variables$smoker.status, ref = "Never smoked")
#brfss.survey.design$variables$diff.walking <- relevel(brfss.survey.design$variables$diff.walking, ref = "No")
#brfss.survey.design$variables$diff.errands.alone <- relevel(brfss.survey.design$variables$diff.errands.alone, ref = "No")
#brfss.survey.design$variables$diff.walking <- relevel(brfss.survey.design$variables$diff.walking, ref = "No")
brfss.survey.design$variables$race.eth <- relevel(brfss.survey.design$variables$race.eth, ref = "Other race only, Non-Hispanic")
brfss.survey.design$variables$age.grp <- relevel(brfss.survey.design$variables$age.grp, ref = "Age 18 to 24")
brfss.survey.design$variables$exercises.yn <- relevel(brfss.survey.design$variables$exercises.yn, ref = "No")
brfss.survey.design$variables$veteran <- relevel(brfss.survey.design$variables$veteran, ref = "Non-Veteran")
brfss.survey.design$variables$diag.nonskin.cancer <- relevel(brfss.survey.design$variables$diag.nonskin.cancer, ref = "No")
brfss.survey.design$variables$current.cancer.treatment <- relevel(brfss.survey.design$variables$current.cancer.treatment, ref = "No - Havent Started")
brfss.survey.design$variables$health.care.coverage.source <- relevel(brfss.survey.design$variables$health.care.coverage.source, ref = "No Coverage")
brfss.survey.design$variables$time.since.care.coverage <- relevel(brfss.survey.design$variables$time.since.care.coverage, ref = "Never")
brfss.survey.design$variables$diabetes <- relevel(brfss.survey.design$variables$diabetes, ref = "No")

# First one here is standard binomial model for surveys
results$stroke.binom <- svyglm(diag.stroke ~ alc.heavy.drinker +  alc.binge + smoker.status + 
                                 n.children + days.anxious + days.sad +
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
                                  days.anxious + diag.depression + diabetes + exercises.yn +
                                  daily.sleep.hrs + sex + edu.level + income + race.eth + age.grp,
                                brfss.survey.design, family = "quasibinomial")

# Calculate OR and 95% CI for estimate
results$stroke.qb.ORCI <- exp(cbind(OR = coef(results$stroke.qbinom), confint(results$stroke.qbinom)))

#KW test to determine if men/women have more strokes
results$kwallis.stroke.by.sex <- svyranktest(diag.stroke ~ sex, design = brfss.survey.design)
results$kwallis.stroke.by.vet.status <- svyranktest(diag.stroke ~ veteran, design = brfss.survey.design)

# Tables ----

# Summary statistics
results$mean.sleep.by.outcome <- svyby(~daily.sleep.hrs, ~diag.stroke, brfss.survey.design, svymean, na.rm = T)
results$mean.sleep.by.outcome$ci <- svyby(~daily.sleep.hrs, ~diag.stroke, brfss.survey.design, svyciprop,vartype="ci",method="beta")

results$mean.bmi.by.outcome <- svyby(~bmi, ~diag.stroke, brfss.survey.design, svymean, na.rm = T, na.rm = T)
results$mean.bmi.by.outcome <- svyby(~bmi, ~diag.stroke, brfss.survey.design, svyciprop,vartype="ci",method="beta")

results$mean.days.anxious.by.outcome <- svyby(~days.anxious, ~diag.stroke, brfss.survey.design, svymean, na.rm = T, na.rm = T)
results$mean.days.anxious.by.outcome <- svyby(~days.anxious, ~diag.stroke, brfss.survey.design, svyciprop,vartype="ci",method="beta")

results$mean.n.drinks.weekly.by.outcome <- svyby(~alc.n.drinks.weekly, ~diag.stroke, brfss.survey.design, svymean, na.rm = T, na.rm = T)
results$mean.n.drinks.weekly.by.outcome <- svyby(~alc.n.drinks.weekly, ~diag.stroke, brfss.survey.design, svyciprop,vartype="ci",method="beta")


# Two-way tables
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

results$stroke.depression <- na.omit(svytable(~diag.stroke + diag.depression, design = brfss.survey.design))
colnames(results$stroke.depression) <- c("No Depression Diagnosis", "Depression Diagnosis")
rownames(results$stroke.depression) <- c("No Stroke Diagnosis", "Stroke Diagnosis")
results$stroke.depression.p <- prop.table(results$stroke.depression, 1)

# Plots ----
results$box.sleep <- svyboxplot(daily.sleep.hrs ~ diag.stroke, design = brfss.survey.design, col = brewer.pal(2, "Set3"), all.outliers = F,
                                xlab = "Outcome", ylab = "Hours of Sleep", main = "Hours of Sleep and Stroke Outcome")
results$box.anxious <- svyboxplot(days.anxious ~ diag.stroke, design = brfss.survey.design, col = brewer.pal(2, "Set3"), all.outliers = F,
                                  xlab = "Outcome", ylab = "Days of Anxiety", main = "Days Affected by Anxiety and Stroke Outcome")
results$box.ndrinks.weekly <- svyboxplot(alc.n.drinks.weekly ~ diag.stroke, design = brfss.survey.design,
                                         col = brewer.pal(2, "Set3"), all.outliers = F,
                                         xlab = "Outcome", ylab = "Number of Drinks Weekly", main = "Number of Alcoholic Drinks Weekly and Stroke Outcome")
# odds errorbar
results$agegrp.odds.bar <- ggplot(ggpredict(results$stroke.binom, "age.grp"), aes(factor(x, 
                                                                                         labels = c("18 to 24",
                                                                                         "25 to 34",
                                                                                         "** 35 to 44",
                                                                                         "** 45 to 54",
                                                                                         "** 55 to 64",
                                                                                         "** 65+")), predicted)) +
  geom_point() +
  geom_errorbar(aes(min=conf.low, max=conf.high)) +
  coord_flip() + 
  theme_bw() +
  ggtitle("AgeGr oup & Stroke Probability Estimate") +
  labs(x="Age Group", y = "Stroke Probability", caption = "* - Significant at α=0.05\n** - Significant at α=0.01") +
  theme(plot.title = element_text(hjust = .50), 
        plot.caption = element_text(hjust=.95),
        panel.border = element_blank(),
        panel.grid.minor.y = element_blank())



# Save the results object ----
saveRDS(results, file = "Data/results.rds")
