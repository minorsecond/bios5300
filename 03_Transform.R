# library(tidyverse)
# library(lubridate)
# library(stringr)
# library(forcats)
library(Hmisc)
library(classInt)
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

# Recode the BRFSS variables according to the codebook.
brfss.survey1 <- brfss.survey1 %>%
  mutate(IDATE = as.Date(IDATE, "%m%d%Y"),
         RACE.ETH = car::recode(X_RACEGR3, "1='White only, Non-Hispanic'; 2='Black only, Non-Hispanic'; 3='Other race only, Non-Hispanic'; 4='Multiracial, Non-Hispanic'; 5='Hispanic'; 9='DK/US/Refused'"),
         RACE.ETH = as.factor(RACE.ETH),
         AGE.GROUP = car::recode(X_AGE_G, "1='Age 18 to 24'; 2='Age 25 to 34'; 3='Age 35 to 44'; 4='Age 45 to 54'; 5='Age 55 to 64'; 6='Age 65 or older'"),
         AGE.GROUP = as.factor(AGE.GROUP),
         INCOME2 = car::recode(INCOME2, "1='<$10,000'; 2='<$15,000'; 3='<$20,000'; 4='<$25,000'; 5='<$35,000'; 6='<$50,000'; 7='<$75,000'; 8='>$75,000'; 77='DK/NS'; 99='Refused'"),
         INCOME2 = as.factor(INCOME2),
         SEX = car::recode(SEX, "1='Male'; 2='Female'; 9='NA'"),
         X_BMI5 = X_BMI5 / 100,
         EDUCA = car::recode(EDUCA, "1='Never attended school or only kindergarten'; 2='Grades 1 through 8 (Elementary) '; 3='Grades 9 through 11 (Some high school) '; 4='Grade 12 or GED (High school graduate) '; 5='College 1 year to 3 years (Some college or technical school)'; 6='College 4 years or more (College graduate) '; 9='Refused'"),
         EDUCA = as.factor(EDUCA),
         N.CHILDREN = car::recode(X_CHLDCNT, "1='No children in household'; 2='One child in household'; 3='Two children in household'; 4='Three children in household'; 5='Four children in household'; 6='Five or more children in household'; 9='DK/NS/Missing'"),
         N.CHILDREN = as.factor(N.CHILDREN),
         GENHLTH = car::recode(GENHLTH, "1='Excellent'; 2='Very good'; 3='Good'; 4='Fair'; 5='Poor'; 7='DK/NS'; 9='Refused'"),
         GENHTL = as.factor(GENHLTH),
         EXERANY2 = car::recode(EXERANY2, "1='Yes'; 2='No'; 7='DK/NS'; 9='Refused'"),
         EXERANY2 = as.factor(EXERANY2),
         SLEPTIM1 = car::recode(SLEPTIM1, "77='DK/NS'; 99 ='Refused'"),
         ADDEPEV2 = car::recode(ADDEPEV2, "1='Yes'; 2='No'; 7='DK/NS'; 9='Refused'"),
         ADDEPEV2 = as.factor(ADDEPEV2),
         SATISFIED = car::recode(LSATISFY, "c(1,2)=1; c(3,4)=0; NA=NA; else=NA"),  # Variable for logistic reg
         SATISFIED = as.factor(SATISFIED),
         LSATISFY = car::recode(LSATISFY, "1='Very satisfied'; 2='Satisfied'; 3='Dissatisfied'; 4='Very dissatisfied'; 7='DK/NS'; 9='Refused'"),
         LSATISFY = as.factor(LSATISFY),
         GOOD.HEALTH = car::recode(X_RFHLTH, "1='Good or better health'; 2='Fair or poor health'; 9='DK/NS/Refused/Missing'"),
         GOOD.HEALTH = as.factor(GOOD.HEALTH),
         MENTHLTH = car::recode(MENTHLTH, "88=0; 99=NA; 77=NA"),
         MENTHLTH = as.numeric(MENTHLTH),
         DAYS.ENERGETIC = car::recode(QLHLTH2, "88='0'; 77='DK/NS'; 99='Refused'"),
         DAYS.ANXIOUS = car::recode(QLSTRES2, "88='0'; 77='DK/NS'; 99='Refused'"),
         DAYS.SAD = car::recode(QLMENTL2, "88='0'; 77='DK/NS'; 99='Refused'"),
         DAYS.PHYS.PREVENTED = car::recode(PAINACT2, "88='0'; 77='DK/NS'; 99='Refused'"),
         CVDSTRK3 = car::recode(CVDSTRK3, "1=1; 2=0; 7=NA; 9=NA"),
         CVDSTRK3 = as.factor(CVDSTRK3),
         MARITAL = car::recode(MARITAL, "1='Married'; 2='Divorced'; 3='Widowed'; 4='Separated'; 5='Never married'; 6='A member of an unmarried couple'; 9='Refused'"),
         MARITAL = as.factor(MARITAL),
         MARIJANA = car::recode(MARIJANA, "77='DK/NS'; 88='0'; 99='Refused'"),
         BINGE.DRINKER = car::recode(X_RFBING5, "1='Yyes'; 2='No'; 9='DK/NS/Refused/Missing'"),
         BINGE.DRINKER = as.factor(BINGE.DRINKER),
         N.ALC.DRINKS.WEEK = car::recode(X_DRNKWEK, "99900='DK/US/Refused/Missing'"),
         HEAVY.DRINKER = car::recode(X_RFDRHV5, "1='No'; 2='Yes'; 9='DK/US/Refused/Missing'"),
         HEAVY.DRINKER = as.factor(HEAVY.DRINKER),
         SMOKER.STATUS = car::recode(X_SMOKER3, "1 = 'Current smoker - now smokes every day'; 2='Current smoker - now smokes some days'; 3='Former smoker'; 4='Never smoked'; 9='DK/Refused/Missing'"),
         SMOKER.STATUS = as.factor(SMOKER.STATUS),
         HIGH.RISK = car::recode(HIVRISK4, "1='Yes'; 2='No'; 7='DK/NS'; 9='Refused'"),
         HIGH.RISK = as.factor(HIGH.RISK),
         DRINK.DRIVE = car::recode(X_DRNKDRV, "1='Have driven after having too much to drink'; 2='Have not driven after having too much to drink'; 9='DK/NS/Refused/Missing'"),
         DRINK.DRIVE = as.factor(DRINK.DRIVE))

# Only keep the variables we need
brfss.survey1 <- brfss.survey1[, c("X_LLCPWT", "X_STSTR", "X_PSU", "STATE", "IDATE", "RACE.ETH", "AGE.GROUP", "INCOME2", "SEX", "X_BMI5", "EDUCA", "N.CHILDREN","GENHLTH", "EXERANY2",
                                   "SLEPTIM1", "ADDEPEV2","SATISFIED", "LSATISFY", "GOOD.HEALTH", "MENTHLTH", "DAYS.ENERGETIC", "DAYS.ANXIOUS", "DAYS.SAD", "DAYS.PHYS.PREVENTED",
                                   "CVDSTRK3","MARITAL", "MARIJANA", "BINGE.DRINKER", "N.ALC.DRINKS.WEEK", "HEAVY.DRINKER", "SMOKER.STATUS", "HIGH.RISK", "DRINK.DRIVE")]
# Split variables into 5 groups using kmeans (faster than Jenks)
seed=5
breaks <- classIntervals(as.numeric(brfss.survey1$MARIJANA), n = 5, style="kmeans")$brks
brfss.survey1$MARIJANA.5GRP <- as.factor(cut(as.numeric(brfss.survey1$MARIJANA), breaks=breaks, labels = as.character(1:5)))


# Flush memory
gc()

# Create the survey design
brfss.survey.design <- svydesign(
  nest = T,
  ids=~X_PSU, 
  strata=~X_STSTR, 
  weights=~X_LLCPWT, 
  data = brfss.survey1)

#  Don't need these now. Save RAM.
rm(brfss.survey1)
rm(seed)
rm(breaks)

# Save the transformed data ----
saveRDS(brfss.survey.design, "Data/transformed.rds")
