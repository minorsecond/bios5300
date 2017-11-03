source("./01_import.R")

# numeric cols only
nums <- sapply(ourData, is.numeric)
all_var_corr <- bigcor(ourData[, nums], nblocks = 2, verbose = TRUE)
