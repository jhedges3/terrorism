# top ---------------------------------------------------------------------

rm(list = ls())

# install.packages("dplyr")
# install.packages("plyr")
# install.packages("e1071")
# install.packages("pastecs")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("arules")
# install.packages("MASS")
# install.packages("vcd")
# install.packages("prettyR")
# install.packages("data.table")
# install.packages("descr")
library(dplyr)
library(plyr)
library(e1071)
library(pastecs)
library(ggplot2)
library(lubridate)
library(arules)
library(MASS)
library(vcd)
library(prettyR)
library(data.table)
library(descr)

#' Get the selected data, saved in 02-selection.R
data <- readRDS("data.rds")

#' Set output path
path.output <- "/Users/jameshedges/Documents/Projects/terrorism/output"

# 04-001-01-outcome -------------------------------------------------------

#' Make outcome variables based on nkill
#' (1) kills greater than 0
#' 1=yes, >0 kills
#' 2-no, 0 kills
#' (2) kills, log of
ind.nkill.gt0 <-
  data$ind.nkill!=1

y1.nkill.gt0 <-
  as.numeric(ind.nkill.gt0)

nkill.log <-
  log(as.numeric(unlist(data[which(ind.nkill.gt0), "nkill"])))
y2.nkill.log <-
  rep(NA, length(ind.nkill.gt0))
y2.nkill.log <-
  replace(y2.nkill.log, which(ind.nkill.gt0), nkill.log)

data <-
  mutate(data, y1.nkill.gt0=y1.nkill.gt0, y2.nkill.log=y2.nkill.log)

# 04-001-02-plots 0/1 -----------------------------------------------------

#' Crosstabs of subset of predictors
fig.name.pre <- "04-001-02"
vars.crosstab <- list(
  c("iyear", "y1.nkill.gt0"),
  c("extended", "y1.nkill.gt0"),
  c("region", "y1.nkill.gt0"),
  c("multiple", "y1.nkill.gt0"),
  c("suicide", "y1.nkill.gt0"),
  c("attacktype1", "y1.nkill.gt0"),
  c("claimed", "y1.nkill.gt0"),
  c("weaptype1", "y1.nkill.gt0")
)
for (i in vars.crosstab) {
    fig.name <- paste(path.output,
                      paste(
                        paste(fig.name.pre,
                              paste(toupper(i), collapse="-"), sep="-"),
                        "pdf", sep="."),
                      sep="/")
    pdf(file=fig.name)
  crosstab(data[[i[[1]]]], data[[i[[2]]]],
           xlab=i[[1]], ylab=i[[2]])
    dev.off()
}
#' (1) XXX ADD SUMMARY


# bottom ------------------------------------------------------------------

#' Save the data
# saveRDS(data, "data.rds")