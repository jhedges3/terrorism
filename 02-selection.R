install.packages("dplyr")
library(dplyr)

#' Get the cleaned data, saved in 01-clean.r
data.06to13 <- 
  readRDS("data.06to13.rds")

#' (1) doubtterr == 0
#' X(2) date uncertain == 0
#' (3) success == 1
#' (4) weapon type != 13 (unknown)
#' X(5) nperps != -99 | nperps != “Unknown"
data.06to13.select <- 
  filter(
    data.06to13, 
    doubtterr == 0, 
    success == 1,
    weaptype1 != 13
    )

#' Save the data
saveRDS(data.06to13.select, "data.06to13.select.rds")