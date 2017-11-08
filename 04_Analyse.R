#library(tidyverse)
# library(broom)
# library(modelr)
library(ggplot2)
library(ggeffects)
library(srvyr)
library(survey)
library(RColorBrewer)
library(tigris)  # for state info
source("./Functions/pub_graphs.R")
options(survey.lonely.psu = "certainty")

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
brfss.survey.design$variables$diag.depression <- relevel(brfss.survey.design$variables$diag.depression, ref = "No")
brfss.survey.design$variables$smoker.status <- relevel(brfss.survey.design$variables$smoker.status, ref = "Never smoked")
brfss.survey.design$variables$race.eth <- relevel(brfss.survey.design$variables$race.eth, ref = "Other race only, Non-Hispanic")
brfss.survey.design$variables$age.grp <- relevel(brfss.survey.design$variables$age.grp, ref = "Age 18 to 24")
brfss.survey.design$variables$edu.level <- relevel(brfss.survey.design$variables$edu.level, ref = "Never attended school or only kindergarten")
brfss.survey.design$variables$exercises.yn <- relevel(brfss.survey.design$variables$exercises.yn, ref = "No")
brfss.survey.design$variables$veteran <- relevel(brfss.survey.design$variables$veteran, ref = "Non-Veteran")
brfss.survey.design$variables$diag.nonskin.cancer <- relevel(brfss.survey.design$variables$diag.nonskin.cancer, ref = "No")
brfss.survey.design$variables$current.cancer.treatment <- relevel(brfss.survey.design$variables$current.cancer.treatment, ref = "No - Havent Started")
brfss.survey.design$variables$health.care.coverage.source <- relevel(brfss.survey.design$variables$health.care.coverage.source, ref = "No Coverage")
brfss.survey.design$variables$time.since.care.coverage <- relevel(brfss.survey.design$variables$time.since.care.coverage, ref = "Never")
brfss.survey.design$variables$diabetes <- relevel(brfss.survey.design$variables$diabetes, ref = "No")

# Logistic regression on stroke diagnosis status
results$stroke.qbinom <- svyglm(diag.stroke ~ alc.heavy.drinker + smoker.status + 
                                  days.anxious + diag.depression + diabetes  + veteran +
                                  daily.sleep.hrs + sex + edu.level + income + race.eth + age.grp,
                                brfss.survey.design, family = "quasibinomial")

# Calculate OR and 95% CI for estimate
results$stroke.qb.ORCI <- exp(cbind(OR = coef(results$stroke.qbinom), confint(results$stroke.qbinom)))

#KW test to determine if men/women have more strokes
results$kwallis.stroke.by.sex <- svyranktest(diag.stroke ~ sex, design = brfss.survey.design)
results$kwallis.stroke.by.vet.status <- svyranktest(diag.stroke ~ veteran, design = brfss.survey.design)

# Tables ----

# Summary statistics
results$mean.sleep.by.outcome <- svyby(~daily.sleep.hrs, ~diag.stroke, brfss.survey.design, svyciprop,vartype="ci",method="mean", na.rm = T)
results$mean.bmi.by.outcome <- svyby(~bmi, ~diag.stroke, brfss.survey.design, svyciprop,vartype="ci",method="mean", na.rm = T)
results$mean.days.anxious.by.outcome <- svyby(~days.anxious, ~diag.stroke, brfss.survey.design, svyciprop,vartype="ci",method="mean", na.rm = T)
results$mean.n.drinks.weekly.by.outcome <- svyby(~alc.n.drinks.weekly, ~diag.stroke, brfss.survey.design, svyciprop,vartype="ci",method="mean", na.rm = T)


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
results$stroke.depression.p <- prop.table(results$stroke.depression, 2)

# Plots ----
results$box.sleep <- svyboxplot(daily.sleep.hrs ~ diag.stroke, design = brfss.survey.design, col = brewer.pal(2, "Set3"), all.outliers = F,
                                xlab = "Outcome", ylab = "Hours of Sleep", main = "Hours of Sleep and Stroke Outcome")
results$box.anxious <- svyboxplot(days.anxious ~ diag.stroke, design = brfss.survey.design, col = brewer.pal(2, "Set3"), all.outliers = F,
                                  xlab = "Outcome", ylab = "Days of Anxiety", main = "Days Affected by Anxiety and Stroke Outcome")
results$box.ndrinks.weekly <- svyboxplot(alc.n.drinks.weekly ~ diag.stroke, design = brfss.survey.design,
                                         col = brewer.pal(2, "Set3"), all.outliers = F,
                                         xlab = "Outcome", ylab = "Number of Drinks Weekly", main = "Number of Alcoholic Drinks Weekly and Stroke Outcome")
# Age Group & Stroke Probability Graph
results$agegrp.odds.bar <- ggplot(ggpredict(results$stroke.qbinom, "age.grp"), 
                                  aes(factor(x, labels = c("18 to 24",
                                                           "* 25 to 34",
                                                           "* 35 to 44",
                                                           "* 45 to 54",
                                                           "* 55 to 64",
                                                           "* 65+")), 
                                      predicted)) +
  geom_point() +
  geom_errorbar(aes(min=conf.low, 
                    max=conf.high)) +
  coord_flip() +
  ggtitle("Age Group & Stroke Probability Estimate") +
  labs(x="Age Group", y = "Stroke Probability", 
       caption = "** - Significant at α=0.05") +
  theme_Publication() +
  scale_fill_Publication() +
  scale_colour_Publication()

# Depression diagnosis status Probability Graph
results$depression.odds.bar <- ggplot(ggpredict(results$stroke.qbinom, "diag.depression"), 
                                  aes(factor(x, labels = c("No Depression Diagnosis", "Depression Diagnosis")), 
                                      predicted)) +
  geom_point() +
  geom_errorbar(aes(min=conf.low, 
                    max=conf.high)) +
  coord_flip() +
  ggtitle("Depression & Stroke Probability Estimate") +
  labs(x="Depression Diagnosis", y = "Stroke Probability", 
       caption = "** - Significant at α=0.05") +
  theme_Publication() +
  scale_fill_Publication() +
  scale_colour_Publication()

# Education level probability graph
results$sleep.pred <- ggpredict(results$stroke.qbinom, terms = c("daily.sleep.hrs", "diag.depression"))
results$sleep.pred$group <- car::recode(results$sleep.pred$group, "'No'='No Depression Diagnosis'; 'Yes'='Depression Diagnosis';")

results$sleep.hrs.scatter <- ggplot(results$sleep.pred, 
                                      aes(x, predicted, colour = group)) +
  geom_point(show.legend = F) +
  geom_line(show.legend = F) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, linetype = NA), alpha = 0.15, show.legend = F) +
  facet_wrap(~group) +
  ggtitle("Average Hours of Sleep & Stroke Probability Estimate") +
  labs(x="Hours of Sleep", y = "Stroke Probability", 
       caption = "") +
  theme_Publication() +
  scale_colour_Publication()

# Days anxious probability graph
results$days.anxious.pred <- ggpredict(results$stroke.qbinom, terms = c("days.anxious", "diag.depression"))
results$days.anxious.pred$group <- car::recode(results$days.anxious.pred$group, "'No'='No Depression Diagnosis'; 'Yes'='Depression Diagnosis';")

results$days.anxious.scatter <- ggplot(results$days.anxious.pred, 
                                    aes(x, predicted, colour = group)) +
  geom_point(show.legend = F) +
  geom_line(show.legend = F) +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high, linetype = NA), alpha = 0.15, show.legend = F) +
  facet_wrap(~group) +
  ggtitle("Days Anxious & Stroke Probability Estimate") +
  labs(x="Days Anxious in Past Month", y = "Stroke Probability", 
       caption = "") +
  theme_Publication() +
  scale_colour_Publication()

#  Maps
total.strokes.by.state <- svyby(~diag.stroke, ~STATE, brfss.survey.design, svytotal, na.rm = T)
us_states$STATE <- as.factor(us_states$NAME)
us_states <- merge(us_states, census, "STATE")
us_states <- merge(us_states, total.strokes.by.state, "STATE")
us_states <- na.omit(us_states)
us_states$stroke.prevalence <- (us_states$diag.strokeStroke / us_states$Pop.2016) * 100000
  
# Save the results object ----
saveRDS(results, file = "Data/results.rds")

# Save graphs to ./Output/Graphs
ggsave("age_grp_prob.png", 
       results$agegrp.odds.bar, 
       device = "png", 
       path = "./Output/Graphs/", 
       scale = 1, 
       dpi = 300, 
       width = 10, 
       height = 5, 
       units="in")

ggsave("sleep_depression_prob.png", 
       results$sleep.hrs.scatter, 
       device = "png", 
       path = "./Output/Graphs/", 
       scale = 1, 
       dpi = 300, 
       width = 12, 
       height = 8, 
       units="in")

ggsave("days_anxious_depression_prob.png", 
       results$days.anxious.scatter, 
       device = "png", 
       path = "./Output/Graphs/", 
       scale = 1, 
       dpi = 300, 
       width = 12, 
       height = 8, 
       units="in")
