library(Hmisc)
library(classInt)
library(dplyr)
library(survey)
library(srvyr)
source("./Functions/tukey.R")
options(survey.lonely.psu = "certainty")

# Add state names to DF
state.codes$X_STATE <- state.codes$VALUE
state.codes <- state.codes[!(state.codes$VALUE == ".D" | state.codes$VALUE == ".R"), ] # These vals don't exist in BRFSS
brfss.survey1 <- merge(brfss, state.codes, by = "X_STATE")
brfss.survey1$STATE <- as.factor(brfss.survey1$STATE)

# Garbage cleanup
rm(brfss)
rm(state.codes)
gc()

# Remove unused columns
brfss.survey1 <- brfss.survey1[ , -which(names(brfss.survey1) %in% c("X_STATE", "SEQNO", "VALUE"))]

# Recode the BRFSS variables according to the codebook ----
brfss.survey1 <- brfss.survey1 %>%
  mutate(interview.date = as.Date(IDATE, "%m%d%Y"),
         race.eth = car::recode(X_RACEGR3, "1='White only, Non-Hispanic'; 2='Black only, Non-Hispanic'; 3='Other race only, Non-Hispanic'; 4='Multiracial, Non-Hispanic'; 5='Hispanic'; 9=NA"),
         race.eth = as.factor(race.eth),
         age.grp = car::recode(X_AGE_G, "1='Age 18 to 24'; 2='Age 25 to 34'; 3='Age 35 to 44'; 4='Age 45 to 54'; 5='Age 55 to 64'; 6='Age 65 or older'"),
         age.grp = as.factor(age.grp),
         income = car::recode(INCOME2, "1='<$10,000'; 2='<$15,000'; 3='<$20,000'; 4='<$25,000'; 5='<$35,000'; 6='<$50,000'; 7='<$75,000'; 8='>$75,000'; 77=NA; 99=NA"),
         income = as.factor(income),
         sex = car::recode(SEX, "1='Male'; 2='Female'; 9=NA"),
         sex = as.factor(sex),
         bmi = X_BMI5 / 100,
         edu.level = car::recode(EDUCA, "1='Never attended school or only kindergarten'; 2='Grades 1 through 8 (Elementary) '; 3='Grades 9 through 11 (Some high school) '; 4='Grade 12 or GED (High school graduate) '; 5='College 1 year to 3 years (Some college or technical school)'; 6='College 4 years or more (College graduate)'; 9=NA"),
         edu.level = as.factor(edu.level),
         n.children = car::recode(CHILDREN, "88=0; 99=NA"),
         n.children = as.integer(n.children),
         self.hlth.level = car::recode(GENHLTH, "1='Excellent'; 2='Very good'; 3='Good'; 4='Fair'; 5='Poor'; 7=NA; 9=NA"),
         self.hlth.level = as.factor(self.hlth.level),
         exercises.binary = car::recode(EXERANY2, "1=1; 2=0; 7=NA; 9=NA"),
         exercises.binary = as.integer(exercises.binary),
         diff.walking = car::recode(DIFFWALK, "1='Yes'; 2='No'; 7=NA; 9=NA"),
         diff.walking = as.factor(diff.walking),
         diff.dressing = car::recode(DIFFDRES, "1='Yes'; 2='No'; 7=NA; 9=NA"),
         diff.dressing = as.factor(diff.dressing),
         diff.decisions = car::recode(DECIDE, "1='Yes'; 2='No'; 7=NA; 9=NA"),
         diff.decisions = as.factor(diff.decisions),
         diff.errands.alone = car::recode(DIFFALON, "1='Yes'; 2='No'; 7=NA; 9=NA"),
         diff.errands.alone = as.factor(diff.errands.alone),
         exercises.yn = car::recode(EXERANY2, "1='Yes'; 2='No'; 7=NA; 9=NA"),
         exercises.yn = as.factor(exercises.yn),
         daily.sleep.hrs = car::recode(SLEPTIM1, "77=NA; 99 =NA"),
         daily.sleep.hrs <- as.integer(daily.sleep.hrs),
         diag.depression = car::recode(ADDEPEV2, "1='Yes'; 2='No'; 7=NA; 9=NA"),
         diag.depression = as.factor(diag.depression),
         life.satisfied.binary = car::recode(LSATISFY, "c(1,2)=1; c(3,4)=0; NA=NA; else=NA"),  # Variable for logistic reg
         life.satisfied.binary = as.factor(life.satisfied.binary),
         life.satisfied = car::recode(LSATISFY, "1='Very satisfied'; 2='Satisfied'; 3='Dissatisfied'; 4='Very dissatisfied'; 7=NA; 9=NA"),
         life.satisfied = as.factor(life.satisfied),
         good.health = car::recode(X_RFHLTH, "1='Good or better health'; 2='Fair or poor health'; 9=NA"),
         good.health = as.factor(good.health),
         days.poor.mental.health = car::recode(MENTHLTH, "88=0; 99=NA; 77=NA"),
         days.poor.mental.health = as.numeric(days.poor.mental.health),
         days.energetic = car::recode(QLHLTH2, "88='0'; 77=NA; 99=NA"),
         days.anxious = car::recode(QLSTRES2, "88='0'; 77=NA; 99=NA"),
         days.anxious <- as.integer(days.anxious),
         days.sad = car::recode(QLMENTL2, "88='0'; 77=NA; 99=NA"),
         days.sad <- as.integer(days.sad),
         days.pain.prev.phys.act = car::recode(PAINACT2, "88='0'; 77=NA; 99=NA"),
         diag.stroke = car::recode(CVDSTRK3, "1=1; 2=0; 7=NA; 9=NA"),
         diag.stroke = factor(diag.stroke, labels=c("No Stroke", "Stroke")),
         marital.status = car::recode(MARITAL, "1='Married'; 2='Divorced'; 3='Widowed'; 4='Separated'; 5='Never married'; 6='A member of an unmarried couple'; 9=NA"),
         marital.status = as.factor(marital.status),
         veteran = car::recode(VETERAN3, "1='Yes'; 2='No'; 7=NA; 9=NA"),
         veteran = factor(veteran, labels = c("Non-Veteran", "Veteran")),
         days.use.marij = car::recode(MARIJANA, "77=NA; 88='0'; 99=NA"),
         days.use.marij <- as.integer(days.use.marij),
         alc.binge = car::recode(X_RFBING5, "1='Yyes'; 2='No'; 9=NA"),
         alc.binge = as.factor(alc.binge),
         alc.n.drinks.weekly = car::recode(X_DRNKWEK, "99900=NA"),
         alc.heavy.drinker = car::recode(X_RFDRHV5, "1='No'; 2='Yes'; 9=NA"),
         alc.heavy.drinker = as.factor(alc.heavy.drinker),
         smoker.status = car::recode(X_SMOKER3, "1 = 'Current smoker - now smokes every day'; 2='Current smoker - now smokes some days'; 3='Former smoker'; 4='Never smoked'; 9=NA"),
         smoker.status = as.factor(smoker.status),
         smoker.100.cigs.life = car::recode(SMOKE100, "1='Yes'; 2='No'; 7=NA; 9=NA"),
         smoker.100.cigs.life = as.factor(smoker.100.cigs.life),
         risky.behavior = car::recode(HIVRISK4, "1='Yes'; 2='No'; 7=NA; 9=NA"),
         risky.behavior = as.factor(risky.behavior),
         diag.nonskin.cancer = car::recode(CHCOCNCR, "1='Yes'; 2='No'; 7=NA; 9=NA"),
         diag.nonskin.cancer = as.factor(diag.nonskin.cancer),
         current.cancer.treatment = car::recode(CSRVTRT1, "1='Yes'; 2='No - Completed Treatment'; 3='No - Refused Treatment'; 4='No - Havent Started'; 7=NA; 9=NA"),
         current.cancer.treatment = as.factor(current.cancer.treatment),
         health.care.coverage.source = car::recode(HLTHCVR1, "1='Employer'; 2='Self'; 3='Medicare'; 4='Medicaid'; 5='TRICARE'; 6='Tribal Health Services'; 7='Other'; 8='No Coverage'; 77=NA; 99=NA"),
         health.care.coverage.source = as.factor(health.care.coverage.source),
         time.since.care.coverage = car::recode(LSTCOVRG, "1='<6 Months'; 2='Between 6 months and 1 year'; 3='Between 1 and 3 years'; 4='>3 Years'; 5='Never'; 7=NA; 9=NA"),
         time.since.care.coverage = as.factor(time.since.care.coverage),
         diabetes = car::recode(DIABETE3, "1='Yes'; 2='No'; 3='No'; 4='Noye'; 7=NA; 9=NA"),
         diabetes = factor(diabetes))

# Only keep the variables we need
brfss.survey1 <- brfss.survey1[, c("X_LLCPWT", "X_STSTR", "X_PSU", "STATE", "interview.date", 
                                   "race.eth", "age.grp", "income", "sex", "bmi", "edu.level", 
                                   "n.children", "exercises.yn", "daily.sleep.hrs", "diag.depression", "diag.stroke",
                                   "days.poor.mental.health", "days.anxious", "days.sad", "days.pain.prev.phys.act",
                                   "marital.status", "alc.binge", "alc.n.drinks.weekly", "days.use.marij", "diabetes",
                                   "alc.heavy.drinker", "smoker.status", "smoker.100.cigs.life", "current.cancer.treatment",
                                   "veteran", "diag.nonskin.cancer", "health.care.coverage.source", "time.since.care.coverage")]

# Remove outliers. ----
#$outlierKD(brfss.survey1, bmi)  # no
outlierKD(brfss.survey1, alc.n.drinks.weekly)  # no
outlierKD(brfss.survey1, as.integer(daily.sleep.hrs))  # yes


# Split variables into 5 groups using kmeans (faster than Jenks)
#seed=5
#breaks <- classIntervals(as.numeric(brfss.survey1$MARIJANA), n = 5, style="kmeans")$brks
#brfss.survey1$MARIJANA.5GRP <- as.factor(cut(as.numeric(brfss.survey1$MARIJANA), breaks=breaks, labels = as.character(1:5)))


# Flush memory
gc()

# Create the survey design ----
brfss.survey.design <- svydesign(
  nest = T,
  ids=~X_PSU, 
  strata=~X_STSTR, 
  weights=~X_LLCPWT, 
  data = brfss.survey1
)

#  Don't need these now. Save RAM.
rm(brfss.survey1)

# Save the transformed data ----
saveRDS(brfss.survey.design, "Data/transformed.rds")
