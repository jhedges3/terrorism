install.packages("openxlsx")
library(openxlsx)

#' Load data for 2006 to 2013; going to focus on these years
data.06to13 <- 
  read.xlsx(
    "./data/gtd_06to13_0814dist.xlsx")

#' Change names so _'s go to .'s and all lowercase
names(data.06to13) <- 
  tolower(gsub("_", ".", names(data.06to13)))

#' Make smaller data; keep as tbl class
data.06to13 <-
  tbl_df(
    data.06to13[, c(
      "eventid",      #' 1
      "iyear",        #' 2
      "imonth",       #' 3
      "iday",         #' 4
      "extended",     #' 5
      "country",      #' 6
      "region",       #' 7
      "crit1",        #' 8
      "crit2",        #' 9
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
data.06to13[, cols.to.factor] <- 
  lapply(data.06to13[, cols.to.factor], as.factor)

#' Remove rows with NAs
data.06to13 <- 
  data.06to13[complete.cases(data.06to13), ]

#' Save the data
saveRDS(data.06to13, "data.06to13.rds")