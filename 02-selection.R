# install.packages("dplyr")
library(dplyr)

#' Get the cleaned data, saved in 01-clean.r
data <- readRDS("data.rds")

#' Remove conditions that are unclear or unknown
#' (1) doubtterr: 
#' (2) success:
#' (3) weapontype1:
#' (3) claimed:
data <- filter(data,
               doubtterr!=1,
               success==1,
               weaptype1!=13,
               claimed!=-9)
 
#' Save the data
saveRDS(data, "data.rds")