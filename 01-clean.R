# install.packages("openxlsx")
# install.packages("dplyr")
library(openxlsx)
library(dplyr)

#' Load data for 2006 to 2013; going to focus on these years
data <- read.xlsx("./data/gtd_06to13_0814dist.xlsx")

#' Change names so _'s go to .'s and all lowercase
names(data) <- tolower(gsub("_", ".", names(data)))

#' Make smaller data; keep as tbl class
data <- tbl_df(
  data[, c(
    "eventid",      #' 01
    "iyear",        #' 02
    "imonth",       #' 03
    "iday",         #' 04
    "extended",     #' 05
    "country",      #' 06
    "region",       #' 07
    "crit1",        #' 08
    "crit2",        #' 09
    "crit3",        #' 10
    "doubtterr",    #' 11
    "multiple",     #' 12
    "success",      #' 13
    "suicide",      #' 14
    "attacktype1",  #' 15
    "targtype1",    #' 16
    "natlty1",      #' 17
    "nperps",       #' 18
    "claimed",      #' 19
    "weaptype1",    #' 20
    "nkill",        #' 21
    "nkillus",      #' 22
    "nkillter"      #' 23
  )])

#' Cols to convert to factor; these are really categorical
cols.to.factor <-
  c(
    "extended",
    "country",
    "region",
    "crit1",
    "crit2",
    "crit3",
    "doubtterr",
    "multiple",
    "success",
    "suicide",
    "attacktype1",
    "targtype1",
    "natlty1",
    "claimed",
    "weaptype1"
  )

#' Convert those cols, leave everything else
data[, cols.to.factor] <- lapply(data[, cols.to.factor], as.factor)

#' Remove rows with NAs
data <- data[complete.cases(data), ]

#' Save the data
saveRDS(data, "data.rds")

#' Make data frame tbl of GTD codes
#' use this for fixing labels in legends
list.files(path="/Users/jameshedges/Documents/Projects/terrorism/terrorism")