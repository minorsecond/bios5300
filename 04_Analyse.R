# This script analyzes the data parsed out in 03_Transform.R
# and saves it out to ./Data/results.rds for use in the
# RMarkdown script.

library(ggplot2)
library(ggeffects)
library(srvyr)
library(survey)
library(RColorBrewer)
library(fiftystater)
library(plyr)
library(dplyr)
library(Hmisc)
library(data.table)
library(stargazer)
library(scales)
library(effects)
library(vcd)
source("./Functions/pub_graphs.R")  # Nice graphs

plot_odds<-function(x, title = NULL){
  tmp<-data.frame(cbind(exp(coef(x)), exp(confint(x))))
  odds<-tmp[-1,]
  names(odds)<-c('OR', 'lower', 'upper')
  odds$vars<-row.names(odds)
  ticks<-c(seq(.1, 1, by =.1), seq(0, 10, by =1), seq(10, 100, by =10))
  
  g <- ggplot(odds, aes(y= OR, x = reorder(vars, OR))) +
    geom_point() +
    geom_errorbar(aes(ymin=lower, ymax=upper), width=.2) +
    scale_y_log10(breaks=ticks, labels = ticks) +
    geom_hline(yintercept = 1, linetype=2) +
    coord_flip() +
    labs(title = title, x = 'Variables', y = 'OR') +
    theme_bw()
  
  return(g)
}

# a single-PSU stratum makes no contribution to the variance
# (for multistage sampling it makes no contribution at that
# level of sampling).
options(survey.lonely.psu = "certainty")

# Disable scientific notation
options(scipen = 999)

# Initialize an empty list to store the results ----
results <- list()

# Load the transformed data ----
print("Reading transformed.rds into memory...")
brfss.survey.design <- readRDS("Data/transformed.rds")

# Specific transformations ---- Relevel some of the factors
# so that the regression output is easier to interpret.
print(("Setting logistic regression baseline levels..."))
brfss.survey.design$variables$sex <- relevel(brfss.survey.design$variables$sex, 
  ref = "Male")
brfss.survey.design$variables$diag.depression <- relevel(brfss.survey.design$variables$diag.depression, 
  ref = "No Depression Diagnosis")
brfss.survey.design$variables$smoker.status <- relevel(brfss.survey.design$variables$smoker.status, 
  ref = "Never Smoked")
brfss.survey.design$variables$race.eth <- relevel(brfss.survey.design$variables$race.eth, 
  ref = "Other race only, Non-Hispanic")
brfss.survey.design$variables$age.grp <- relevel(brfss.survey.design$variables$age.grp, 
  ref = "Age 18 to 24")
brfss.survey.design$variables$alc.heavy.drinker <- relevel(brfss.survey.design$variables$alc.heavy.drinker, 
  ref = "No")

# Models ---- Logistic regression on stroke diagnosis status
# ----
print("Running binomial logistic regression...")
results$stroke.binom <- svyglm(diag.stroke ~ alc.heavy.drinker + 
  smoker.status + days.anxious + diag.depression + daily.sleep.hrs + 
  sex + age.grp + race.eth, brfss.survey.design, family = "binomial")

print("Running quasibinomial logistic regression...")
results$stroke.qbinom <- svyglm(diag.stroke ~ alc.heavy.drinker + 
  smoker.status + days.anxious + diag.depression + daily.sleep.hrs + 
  sex + age.grp + race.eth, brfss.survey.design, family = "quasibinomial")

results$stroke.nandySuggestions <- svyglm(diag.stroke ~ alc.heavy.drinker + smoker.status + days.anxious + diag.depression + daily.sleep.hrs + 
                                  sex*daily.sleep.hrs + sex*days.anxious + age.grp + race.eth, brfss.survey.design, family = "quasibinomial")

# Calculate OR and 95% CI for estimate
print("Calculating OR & 95% CI...")
results$stroke.qb.ORCI <- exp(cbind(OR = coef(results$stroke.qbinom), 
  confint(results$stroke.qbinom)))
results$stroke.b.ORCI <- exp(cbind(OR = coef(results$stroke.binom), 
  confint(results$stroke.binom)))
results$stroke.nandy.ORCI <- exp(cbind(OR = coef(results$stroke.nandySuggestions), 
                                   confint(results$stroke.nandySuggestions)))

# Write report to file in ./Output/Reports/logit.html
OR.vector <- exp(results$stroke.qbinom$coef)
CI.vector <- exp(confint(results$stroke.qbinom))
p.values <- summary(results$stroke.qbinom)$coefficients[, 4]
stargazer(results$stroke.qbinom, coef = list(OR.vector), ci = T, 
  ci.custom = list(CI.vector), p = list(p.values), single.row = T, 
  type = "html", title = "Logistic Regression Model Estimating Effects of Risk Factors & Demographic Variables on Stroke Outcome Odds", 
  out = "./Output/Reports/logit.html", omit = c("Constant"))

# Tables ----

# Summary statistics
print("Calculating mean values for sleep, days anxious and total strokes.")
results$mean.sleep.by.outcome <- svyby(~daily.sleep.hrs, ~diag.stroke, 
  brfss.survey.design, svyciprop, vartype = "ci", method = "mean", 
  na.rm = T)
results$mean.days.anxious.by.outcome <- svyby(~days.anxious, 
  ~diag.stroke, brfss.survey.design, svyciprop, vartype = "ci", 
  method = "mean", na.rm = T)
results$total.strokes <- svytotal(~diag.stroke, design = brfss.survey.design, 
  na.rm = T)

# Two-way tables & Chi Sq. Tests----
print("Creating two-way tables...")
results$stroke.sex <- na.omit(svytable(~diag.stroke + sex, design = brfss.survey.design))
colnames(results$stroke.sex) <- c("Male", "Female")
rownames(results$stroke.sex) <- c("No stroke diagnosis", "Stroke diagnosis")
results$stroke.sex.p <- prop.table(results$stroke.sex, 2)
results$chsq.by.sex <- svychisq(~diag.stroke + sex, design = brfss.survey.design)

results$stroke.heavy.drinker <- na.omit(svytable(~diag.stroke + 
  alc.heavy.drinker, design = brfss.survey.design))
colnames(results$stroke.heavy.drinker) <- c("Non-heavy Drinker", 
  "Heavy Drinker")
rownames(results$stroke.heavy.drinker) <- c("No stroke diagnosis", 
  "Stroke diagnosis")
results$stroke.heavy.drinker.p <- prop.table(results$stroke.heavy.drinker, 
  2)

results$stroke.depression <- na.omit(svytable(~diag.stroke + 
  diag.depression, design = brfss.survey.design))
colnames(results$stroke.depression) <- c("No Depression Diagnosis", 
  "Depression Diagnosis")
rownames(results$stroke.depression) <- c("No Stroke Diagnosis", 
  "Stroke Diagnosis")
results$stroke.depression.p <- prop.table(results$stroke.depression, 
  1)
results$chsq.by.depression <- svychisq(~diag.stroke + diag.depression, 
  design = brfss.survey.design)

results$stroke.smoker <- na.omit(svytable(~diag.stroke + smoker.status, 
  design = brfss.survey.design))

results$stroke.smoker.p <- prop.table(results$stroke.smoker, 
  1)
results$chsq.by.smoker <- svychisq(~diag.stroke + smoker.status, 
  design = brfss.survey.design, statistic = "Chisq")

# Plots ----
plot_odds(results$stroke.qbinom)

print("Creating boxplots...")
results$box.sleep <- svyboxplot(daily.sleep.hrs ~ diag.stroke, 
  design = brfss.survey.design, col = brewer.pal(2, "Set3"), 
  all.outliers = F, xlab = "Outcome", ylab = "Hours of Sleep", 
  main = "Hours of Sleep and Stroke Outcome")

results$box.anxious <- svyboxplot(days.anxious ~ diag.stroke, 
  design = brfss.survey.design, col = brewer.pal(2, "Set3"), 
  all.outliers = F, xlab = "Outcome", ylab = "Days of Anxiety", 
  main = "Days Affected by Anxiety and Stroke Outcome")

# Effects
print("Creating effects plots...")
results$effects.age.income.sex <- Effect(focal.predictors = c("income", 
  "age.grp", "sex"), mod = results$stroke.qbinom)

results$effects.age.sex <- Effect(focal.predictors = c("age.grp", 
  "sex", "diag.depression"), mod = results$stroke.qbinom)

results$effects.anxiety.depression <- Effect(focal.predictors = c("days.anxious", 
  "sex", "diag.depression"), mod = results$stroke.qbinom)

results$effects.sleep.depression <- Effect(focal.predictors = c("daily.sleep.hrs", 
                                                                  "smoker.status", "diag.depression"), mod = results$stroke.qbinom)

results$effects.anxiety.depression.smoking <- Effect(focal.predictors = c("days.anxious", 
  "smoker.status", "diag.depression"), mod = results$stroke.qbinom)

# Age Group & Stroke Probability Graph
print("Creating graphs...")
results$agegrp.odds.bar <- ggplot(ggpredict(results$stroke.qbinom, 
  "age.grp"), aes(factor(x, labels = c("18 to 24", "** 25 to 34", 
  "** 35 to 44", "** 45 to 54", "** 55 to 64", "** 65+")), 
  predicted)) + geom_point() + geom_errorbar(aes(min = conf.low, 
  max = conf.high)) + coord_flip() + labs(x = "Age Group", 
  y = "Stroke Probability", caption = "** - Significant at α=0.05") + 
  theme_Publication() + scale_fill_Publication() + scale_colour_Publication()

# Depression diagnosis status Probability Graph
results$depression.odds.bar <- ggplot(ggpredict(results$stroke.qbinom, 
  "diag.depression"), aes(factor(x, labels = c("No Depression Diagnosis", 
  "Depression Diagnosis")), predicted)) + geom_point() + geom_errorbar(aes(min = conf.low, 
  max = conf.high)) + coord_flip() + ggtitle("Depression & Stroke Probability Estimate") + 
  labs(x = "Depression Diagnosis", y = "Stroke Probability", 
    caption = "** - Significant at α=0.05") + theme_Publication() + 
  scale_fill_Publication() + scale_colour_Publication()

# Income Probability Graph
results$income.prob.graph <- ggplot(ggpredict(results$stroke.qbinom, 
  terms = c("income", "diag.depression")), aes(factor(x, labels = c("Income < $10,000", 
  "Income < $15,000", "Income < $20,000", "Income < $25,000", 
  "Income < $35,000", "Income < $50,000", "Income < $75,000", 
  "Income > $75,000")), predicted, colour = group)) + geom_point() + 
  geom_errorbar(aes(min = conf.low, max = conf.high)) + coord_flip() + 
  ggtitle("Depression & Stroke Probability Estimate") + labs(x = "Depression Diagnosis", 
  y = "Stroke Probability", caption = "** - Significant at α=0.05") + 
  theme_Publication() + facet_wrap(~group) + scale_fill_Publication() + 
  scale_colour_Publication()

# Education level probability graph
results$sleep.pred <- ggpredict(results$stroke.qbinom, terms = c("daily.sleep.hrs", 
  "diag.depression"))
results$sleep.pred$group <- car::recode(results$sleep.pred$group, 
  "'No'='No Depression Diagnosis'; 'Yes'='Depression Diagnosis';")

results$sleep.hrs.scatter <- ggplot(results$sleep.pred, aes(x, 
  predicted, colour = group)) + geom_point(show.legend = F) + 
  geom_line(show.legend = F) + geom_ribbon(aes(ymin = conf.low, 
  ymax = conf.high, linetype = NA), alpha = 0.15, show.legend = F) + 
  facet_wrap(~group) + ggtitle("Average Hours of Sleep & Stroke Probability Estimate") + 
  labs(x = "Hours of Sleep", y = "Stroke Probability", caption = "") + 
  theme_Publication() + scale_colour_Publication()

# Days anxious probability graph
results$days.anxious.pred <- ggpredict(results$stroke.qbinom, 
  terms = c("days.anxious", "diag.depression"))
results$days.anxious.pred$group <- car::recode(results$days.anxious.pred$group, 
  "'No'='No Depression Diagnosis'; 'Yes'='Depression Diagnosis';")

results$days.anxious.scatter <- ggplot(results$days.anxious.pred, 
  aes(x, predicted, colour = group)) + geom_point(show.legend = F) + 
  geom_line(show.legend = F) + geom_ribbon(aes(ymin = conf.low, 
  ymax = conf.high, linetype = NA), alpha = 0.15, show.legend = F) + 
  facet_wrap(~group) + ggtitle("Days Anxious & Stroke Probability Estimate") + 
  labs(x = "Days Anxious in Past Month", y = "Stroke Probability", 
    caption = "") + theme_Publication() + scale_colour_Publication()

# Income group bargraph
results$total.income.groups <- svytotal(~income, design = brfss.survey.design, 
  na.rm = T)
results$total.income.groups <- melt(results$total.income.groups)
colnames(results$total.income.groups) <- c("Total", "SE")
results$total.income.groups <- setDT(results$total.income.groups, 
  keep.rownames = TRUE)[]

results$bargraph_income <- ggplot(results$total.income.groups, 
  aes(rn, Total)) + geom_bar(stat = "identity") + theme_Publication() + 
  scale_colour_Publication() + scale_y_continuous(label = comma) + 
  scale_x_discrete(labels = c("<$10,000", "$10,000 - $15,000", 
    "$15,000 - $20,000", "$20,000 - $25,000", "$25,000 - $35,000", 
    "$35,000 - $50,000", "$50,000 - $75,000", ">$75,000")) + 
  ggtitle("2016 United States Household Income Ranges", subtitle = "Weighted BRFSS Data") + 
  labs(x = "Income Range")

# Age group bargraph
results$total.age.groups <- svytotal(~age.grp, design = brfss.survey.design, 
  na.rm = T)

results$total.age.groups <- melt(results$total.age.groups)
colnames(results$total.age.groups) <- c("Total", "SE")
results$total.age.groups <- setDT(results$total.age.groups, keep.rownames = TRUE)[]

results$bargraph_age.grp <- ggplot(results$total.age.groups, 
  aes(rn, Total)) + geom_bar(stat = "identity") + theme_Publication() + 
  scale_colour_Publication() + scale_y_continuous(label = comma) + 
  scale_x_discrete(labels = c("18 to 24 Years Old", "25 to 34 Years Old", 
    "35 to 44 Years Old", "45 to 54 Years Old", "55 to 64 Years Old", 
    "65+ Years Old")) + ggtitle("2016 United States Age Groups", 
  subtitle = "Weighted BRFSS Data") + labs(x = "Age Group")



# Mosaic Plots
print("Creating mosaic plot...")
mosaic(results$stroke.smoker, shade = T, main = "Proportions of Stroke Diagnoses by Smoker Status", 
  labeling_args = list(set_varnames = c(diag.stroke = "Stroke Outcome", 
    smoker.status = "Smoker Status")), spacing = spacing_equal(unit(1, "lines")))

# Maps -------------------
print("Creating map...")
results$total.strokes.by.state <- svyby(~diag.stroke, ~STATE, 
  brfss.survey.design, svytotal, na.rm = T)

results$total.strokes.by.state$STATE <- tolower(results$total.strokes.by.state$STATE)
us_states$STATE <- as.factor(tolower(us_states$NAME))
census$STATE <- tolower(census$STATE)
us_states$id <- rownames(us_states@data)
us_states@data <- join(as.data.frame(us_states), census, "STATE")
us_states@data <- join(as.data.frame(us_states), results$total.strokes.by.state, 
  "STATE")
us_states$id <- rownames(us_states@data)
us_states$stroke.prevalence <- (us_states$diag.strokeStroke/us_states$Pop.2016) * 
  1e+05
stroke.prevalence <- na.omit(us_states[, c("STATE", "stroke.prevalence")]@data)
stroke.prevalence$quintile <- with(stroke.prevalence, cut(stroke.prevalence, 
  breaks = quantile(stroke.prevalence, probs = seq(0, 1, by = 0.2), 
    na.rm = TRUE), include.lowest = TRUE, dig.lab = 5))
us_states.gg <- fortify(us_states)
us_states.gg <- join(us_states.gg, us_states@data, by = "id")

results$colorPalette <- brewer.pal(5, name = "YlGn")
results$map.stroke.prevalence <- ggplot(stroke.prevalence, aes(map_id = STATE)) + 
  geom_map(aes(fill = quintile), map = fifty_states, show.legend = T, 
    colour = "grey25", size = 0.25) + expand_limits(x = fifty_states$long, 
  y = fifty_states$lat) + coord_map() + scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) + labs(x = "", y = "") + 
  theme(legend.position = "bottom", panel.background = element_blank()) + 
  theme_Publication() + fifty_states_inset_boxes() + ggtitle("2016 United States Stroke Prevalence", 
  subtitle = "Weighted BRFSS Data") + scale_fill_manual(values = results$colorPalette, 
  name = "Strokes / 100,000", guide = guide_legend(keyheight = unit(4, 
    units = "mm"), keywidth = unit(4, units = "mm"), title.position = "top", 
    reverse = F), labels = c("1512.6 - 2057.2    ", "2057.2 - 2221.6    ", 
    "2221.6 - 2528.8    ", "2528.8 - 2987.0    ", "2987.0 - 3766.6    "))

# Save the results object ----
print("Saving all results...")
saveRDS(results, file = "Data/results.rds")

# Save graphs to ./Output/Graphs
ggsave("age_grp_prob.png", results$agegrp.odds.bar, device = "png", 
  path = "./Output/Graphs/", scale = 1, dpi = 300, width = 10, 
  height = 5, units = "in")

ggsave("sleep_depression_prob.png", results$sleep.hrs.scatter, 
  device = "png", path = "./Output/Graphs/", scale = 1, dpi = 300, 
  width = 12, height = 8, units = "in")

ggsave("days_anxious_depression_prob.png", results$days.anxious.scatter, 
  device = "png", path = "./Output/Graphs/", scale = 1, dpi = 300, 
  width = 12, height = 8, units = "in")

ggsave("stroke_prev_map.png", results$map.stroke.prevalence, 
  device = "png", path = "./Output/Maps/", scale = 1, dpi = 300, 
  width = 12, height = 8, units = "in")

ggsave("income_bargraph.png", results$bargraph_income, device = "png", 
  path = "./Output/Graphs/", scale = 1, dpi = 300, width = 12, 
  height = 8, units = "in")

ggsave("age_bargraph.png", results$bargraph_age.grp, device = "png", 
  path = "./Output/Graphs/", scale = 1, dpi = 300, width = 12, 
  height = 8, units = "in")


