source("./Functions/detachAll.R")
library(foreign)
library(tigris)
library(openxlsx)
library(fiftystater)

# Download the 2016 BRFSS Data from CDC ----
OUTFILE = "./Data/Raw/LLCP2016.xpt "
URL <- "https://www.cdc.gov/brfss/annual_data/2016/files/LLCP2016XPT.zip"
if (!file.exists(OUTFILE)) {
  ZIPFILE = "./Data/Raw/LLCP2016XPT.zip"
  download.file(URL, ZIPFILE, method="auto")
  unzip(zipfile = ZIPFILE, overwrite = TRUE)  # Extract data
  file.remove(ZIPFILE)  # Delete zipfile
  rm(ZIPFILE)  # Clean up cruft
  brfss <- read.xport(OUTFILE) #Read in the data
  rm(OUTFILE)
  rm(URL)
  file.remove(ZIPFILE)
} else {
  brfss <- read.xport(OUTFILE)
  rm(OUTFILE)
  rm(URL)}

# Get CSV file of BRFSS state designations ----
CSVPATH <- "./Data/Raw/brfss_state.csv"
download.file("https://raw.githubusercontent.com/arilamstein/complexsurvey/master/brfss_state.csv", CSVPATH)
state.codes <- read.csv(CSVPATH)
rm(CSVPATH)

# Get the census data ----
CENSUS_PATH <- "./Data/Raw/population.xlsx"
download.file("https://www2.census.gov/programs-surveys/popest/tables/2010-2016/state/totals/nst-est2016-01.xlsx", CENSUS_PATH)
census <- readWorkbook(CENSUS_PATH, sheet = 1, startRow = 1, colNames = TRUE,
                       rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                       skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                       namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE)

census <- census[-c(seq(1,8)), ]  # Remove census rows 1-8
census <- census[-c(seq(53,57)), ]  # Remove census rows 53-57
census <- census[ , c(1, 10)]  # Only keep state name column and 2016 population column
colnames(census) <- c("STATE", "Pop.2016")  # Replace column names
census$STATE <- substring(census$STATE, 2)  # Remove leading '.' character from state names

rm(CENSUS_PATH)

# Get the state data ----
us_states <- states(year = 2016)

gc()