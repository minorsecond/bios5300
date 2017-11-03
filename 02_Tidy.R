#library(tidyverse)
# library(lubridate)
# library(stringr)
# library(forcats)

# Load the csv data and tidy it ----
#read_csv("Data/.csv") %>%
#  select() %>%
#  filter() %>%
#  mutate() %>%
#  dmap_if(is.character, factor) %>%

#Rename brfss columns
brfss.cleaned <- brfss
#colnames(brfss.cleaned) <- c("state", "file.month", "interview.date",
#                             "interview.month", "interview.day", "interview.year",
#                             "final.disposition", "ann.seq.number", "prim.sample.unit",
#                             "correct.lline.num", "private.residence", "college.housing",
#                             "res.of.state", "cell.tele", "over.18.yoa",
#                             "n.adults.hshld", "n.adult.men", "n.adult.women",
#                             "correct.cell.num", "is.this.cell.phne", "cell.over.18.yoa",
#                             "also.have.landline", )

# Save the tidy-ed data ----
  saveRDS(file = "Data/tidy.rds")
