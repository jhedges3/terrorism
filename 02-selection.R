# install.packages("dplyr")
library(dplyr)

#' Get the cleaned data, saved in 01-clean.r
data <-readRDS("data.rds")

#' (1) doubtterr == 0
#' (2) success == 1
#' (3) weapon type != 13 (unknown)
data.select <- 
  filter(
    data, 
    doubtterr == 0, 
    success == 1,
    weaptype1 != 13
    )

#' Save the data
saveRDS(data.select, "data.select.rds")