#library(tidyverse)
library(foreign)
#library(readxl)
#library(haven)
#library(rvest)
#library(sonlite)

# Download the 2016 BRFSS Data from CDCif(!file.exists(destfile)){
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

# Get CSV file of BRFSS state designations
CSVPATH <- "./Data/Raw/brfss_state.csv"
download.file("https://raw.githubusercontent.com/arilamstein/complexsurvey/master/brfss_state.csv", CSVPATH)
state.codes <- read.csv(CSVPATH)
rm(CSVPATH)

gc()