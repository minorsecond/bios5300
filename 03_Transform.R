# This script parses the data downloaded in 01_Import.R,
# creates the survey design object, and saves it to
# ./Data/transformed.rds for use in step 4.

library(Hmisc)
library(classInt)
library(dplyr)
library(survey)
library(srvyr)

# a single-PSU stratum makes no contribution to the variance
# (for multistage sampling it makes no contribution at that
# level of sampling).
options(survey.lonely.psu = "certainty")

# Add state names to DF
state.codes$X_STATE <- state.codes$VALUE
state.codes <- state.codes[!(state.codes$VALUE == ".D" | state.codes$VALUE == 
  ".R"), ]  # These vals don't exist in BRFSS
brfss.survey1 <- merge(brfss, state.codes, by = "X_STATE")
brfss.survey1$STATE <- as.factor(brfss.survey1$STATE)

# Garbage cleanup
rm(brfss)
rm(state.codes)
gc()

# Remove unused columns
brfss.survey1 <- brfss.survey1[, -which(names(brfss.survey1) %in% 
  c("X_STATE", "SEQNO", "VALUE"))]

# Recode the BRFSS variables according to the codebook ----
brfss.survey1 <- brfss.survey1 %>% mutate(race.eth = car::recode(X_RACEGR3, 
  "1='White only, Non-Hispanic'; 2='Black only, Non-Hispanic'; 3='Other race only, Non-Hispanic'; 4='Multiracial, Non-Hispanic'; 5='Hispanic'; 9=NA"), 
  race.eth = as.factor(race.eth), age.grp = car::recode(X_AGE_G, 
    "1='Age 18 to 24'; 2='Age 25 to 34'; 3='Age 35 to 44'; 4='Age 45 to 54'; 5='Age 55 to 64'; 6='Age 65 or older'"), 
  age.grp = as.factor(age.grp), sex = car::recode(SEX, "1='Male'; 2='Female'; 9=NA"), 
  sex = as.factor(sex), daily.sleep.hrs = car::recode(SLEPTIM1, 
    "77=NA; 99 =NA"), daily.sleep.hrs <- as.integer(daily.sleep.hrs), 
  diag.depression = car::recode(ADDEPEV2, "1='Yes'; 2='No'; 7=NA; 9=NA"), 
  diag.depression = factor(diag.depression, labels = c("No Depression Diagnosis", 
    "Depression Diagnosis")), days.anxious = car::recode(QLSTRES2, 
    "88='0'; 77=NA; 99=NA"), days.anxious <- as.integer(days.anxious), 
  diag.stroke = car::recode(CVDSTRK3, "1=1; 2=0; 7=NA; 9=NA"), 
  diag.stroke = factor(diag.stroke, labels = c("No Stroke", 
    "Stroke")), alc.heavy.drinker = car::recode(X_RFDRHV5, 
    "1='No'; 2='Yes'; 9=NA"), alc.heavy.drinker = as.factor(alc.heavy.drinker), 
  smoker.status = car::recode(X_SMOKER3, "1 = 'Daily'; 2='Some days'; 3='Former Smoker'; 4='Never Smoked'; 9=NA"), 
  smoker.status = as.factor(smoker.status))

# Only keep the variables we need
brfss.survey1 <- brfss.survey1[, c("X_LLCPWT", "X_STSTR", "X_PSU", 
  "STATE", "race.eth", "age.grp", "sex", "daily.sleep.hrs", 
  "diag.depression", "diag.stroke", "days.anxious", "alc.heavy.drinker", 
  "smoker.status")]

# Flush memory
gc()

# Create the survey design ----
print("Creating Survey Design Object. This will take some time...")
brfss.survey.design <- svydesign(nest = T, ids = ~X_PSU, strata = ~X_STSTR, 
  weights = ~X_LLCPWT, data = brfss.survey1)

# Don't need these now. Save RAM.
rm(brfss.survey1)

# Save the transformed data ----
saveRDS(brfss.survey.design, "Data/transformed.rds")
