# install.packages("dplyr")
library(dplyr)

#' Get the cleaned data, saved in 01-clean.r
data <- readRDS("data.rds")

#' (1) doubtterr == 0
#' (2) success == 1
#' (3) weapon type != 13 (unknown)
data <- filter(data,
                      doubtterr!=1,
                      success==1,
                      weaptype1!=13,
                      claimed!=-9)
 
#' Save the data
saveRDS(data, "data.rds")