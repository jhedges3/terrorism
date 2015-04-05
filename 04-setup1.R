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
# install.packages("caret")
install.packages("aod")
library(dplyr)
library(plyr)
# library(e1071)
# library(pastecs)
# library(ggplot2)
# library(lubridate)
# library(arules)
# library(MASS)
# library(vcd)
# library(prettyR)
# library(data.table)
# library(descr)
library(caret)
library(aod)

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

# 04-001-02-plots ---------------------------------------------------------

#' Mosaic plots to y1 (greater than 0 kills)
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

#' Box plots to y2 (for gt0 kills, log(kills))
fig.name.pre <- "04-001-02"
vars.crosstab <- list(
  c("iyear", "y2.nkill.log"),
  c("extended", "y2.nkill.log"),
  c("region", "y2.nkill.log"),
  c("multiple", "y2.nkill.log"),
  c("suicide", "y2.nkill.log"),
  c("attacktype1", "y2.nkill.log"),
  c("claimed", "y2.nkill.log"),
  c("weaptype1", "y2.nkill.log")
)
for (i in vars.crosstab) {
  fig.name <- paste(path.output,
                    paste(
                      paste(fig.name.pre,
                            paste(toupper(i), collapse="-"), sep="-"),
                      "pdf", sep="."),
                    sep="/")
  pdf(file=fig.name)
  boxplot(get(i[[2]])~get(i[[1]]), data=data, xlab=i[[1]], ylab=i[[2]])
  dev.off()
}
#' (1) XXX ADD SUMMARY

# 04-002-01-partition -----------------------------------------------------

set.seed(3456)
ind.train <- createDataPartition(data$y1.nkill.gt0, p=0.7, list=FALSE, times=1)
head(ind.train)
data.train <- data[ind.train,]
data.test <- data[-ind.train,]

# 04-003-01-logistic ------------------------------------------------------

mod.logit = glm(
  y1.nkill.gt0 ~ 
    extended +
    region +
    suicide +
    attacktype1 +
    claimed +
    weaptype1,
  family=binomial(logit),
  data=data.train)
summary(mod.logit)
# Call:
#   glm(formula = y1.nkill.gt0 ~ extended + region + suicide + attacktype1 + 
#         claimed + weaptype1, family = binomial(logit), data = data.train)
# 
# Deviance Residuals: 
#   Min      1Q  Median      3Q     Max  
# -3.645  -0.891   0.051   0.786   3.259  
# 
# Coefficients:
# Estimate Std. Error z value             Pr(>|z|)    
# (Intercept)    2.989335   1.289577    2.32              0.02045 *  
# extended1     -0.396945   0.136897   -2.90              0.00374 ** 
# region2       -0.238416   0.742793   -0.32              0.74823    
# region3        0.424098   0.407504    1.04              0.29801    
# region4        2.267816   0.600009    3.78              0.00016 ***
# region5        0.660312   0.392815    1.68              0.09277 .  
# region6        1.030795   0.389407    2.65              0.00812 ** 
# region7       -1.272810   1.087317   -1.17              0.24176    
# region8       -1.655852   0.466763   -3.55              0.00039 ***
# region9       -0.759227   0.620892   -1.22              0.22141    
# region10       1.554028   0.389987    3.98    0.000067530254832 ***
# region11       1.610873   0.393586    4.09    0.000042618149524 ***
# region12       0.430974   0.402491    1.07              0.28427    
# region13      -0.000891   1.350251    0.00              0.99947    
# suicide1       6.835571   0.722795    9.46 < 0.0000000000000002 ***
# attacktype12  -4.383903   0.583412   -7.51    0.000000000000057 ***
# attacktype13  -5.356724   0.589194   -9.09 < 0.0000000000000002 ***
# attacktype14  -8.180578   0.854953   -9.57 < 0.0000000000000002 ***
# attacktype15  -6.518312   0.776560   -8.39 < 0.0000000000000002 ***
# attacktype16  -5.590092   0.596050   -9.38 < 0.0000000000000002 ***
# attacktype17  -7.192484   0.604338  -11.90 < 0.0000000000000002 ***
# attacktype18  -7.308572   0.674997  -10.83 < 0.0000000000000002 ***
# attacktype19   7.282968 137.910031    0.05              0.95788    
# claimed1       0.559636   0.054851   10.20 < 0.0000000000000002 ***
# weaptype15     1.751979   1.080778    1.62              0.10501    
# weaptype16     0.618216   1.088424    0.57              0.57004    
# weaptype17    -6.805785 196.971634   -0.03              0.97244    
# weaptype18    -0.005351   1.090670    0.00              0.99609    
# weaptype19     2.352771   1.068585    2.20              0.02768 *  
# weaptype110    1.503176   1.448941    1.04              0.29954    
# weaptype111   -0.413271   1.509714   -0.27              0.78428    
# weaptype112    1.846236   1.314407    1.40              0.16014    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 28088  on 20339  degrees of freedom
# Residual deviance: 20683  on 20308  degrees of freedom
# AIC: 20747
# 
# Number of Fisher Scoring iterations: 10

# 04-004-01-conf ints -----------------------------------------------------

confint(mod.logit)
# 2.5 % 97.5 %
# (Intercept)   -0.171   5.34 *
# extended1     -0.665  -0.13 **
# region2       -1.762   1.18
# region3       -0.350   1.25
# region4        1.124   3.49 *** East Asia
# region5       -0.083   1.46 .
# region6        0.295   1.83 **  South Asia
# region7       -3.535   0.76
# region8       -2.563  -0.73 *** Western Europe
# region9       -2.021   0.44
# region10       0.817   2.35 *** Middle East & North Africa
# region11       0.866   2.42 *** Sub-Saharan Africa
# region12      -0.333   1.25
# region13      -3.243   2.40
# suicide1       5.674   8.65 ***
# attacktype12  -5.784  -3.42 *** Armed Assault
# attacktype13  -6.764  -4.37 *** Bombing/Explosion
# attacktype14 -10.084  -6.67 *** Hijacking
# attacktype15  -8.218  -5.11 *** Hostage Taking (Barricade Incident)
# attacktype16  -7.007  -4.59 *** Hostage Taking (Kidnapping)
# attacktype17  -8.621  -6.17 *** Facility/Infrastructure Attack
# attacktype18  -8.838  -6.12 *** Unarmed Assault
# attacktype19 -14.185     NA
# claimed1       0.452   0.67 ***
# weaptype15     0.018   4.69
# weaptype16    -1.137   3.57
# weaptype17        NA  27.00
# weaptype18    -1.767   2.95
# weaptype19     0.654   5.28 *   Melee
# weaptype110   -1.391   4.82
# weaptype111   -3.772   2.94
# weaptype112   -0.554   5.04

confint.default(mod.logit)

exp(cbind(OR=coef(mod.logit), confint(mod.logit)))
# OR        2.5 %            97.5 %
# (Intercept)    19.87247   0.84260327          207.7527
# extended1       0.67237   0.51401828            0.8793
# region2         0.78788   0.17161847            3.2589
# region3         1.52821   0.70445279            3.5042
# region4         9.65829   3.07570210           32.6959
# region5         1.93540   0.92013960            4.3226
# region6         2.80329   1.34245578            6.2234
# region7         0.28004   0.02916424            2.1486
# region8         0.19093   0.07703580            0.4843
# region9         0.46803   0.13251168            1.5464
# region10        4.73049   2.26274417           10.5135
# region11        5.00718   2.37709366           11.2003
# region12        1.53876   0.71684169            3.4968
# region13        0.99911   0.03905851           10.9689
# suicide1      930.35987 291.05424516         5718.5431
# attacktype12    0.01248   0.00307649            0.0328
# attacktype13    0.00472   0.00115421            0.0126
# attacktype14    0.00028   0.00004176            0.0013
# attacktype15    0.00148   0.00026964            0.0060
# attacktype16    0.00373   0.00090558            0.0102
# attacktype17    0.00075   0.00018034            0.0021
# attacktype18    0.00067   0.00014508            0.0022
# attacktype19 1455.30148   0.00000069                NA
# claimed1        1.75004   1.57200131            1.9491
# weaptype15      5.76600   1.01835220          109.3549
# weaptype16      1.85561   0.32074356           35.4727
# weaptype17      0.00111           NA 529705868740.7859
# weaptype18      0.99466   0.17083183           19.0583
# weaptype19     10.51466   1.92276007          196.9670
# weaptype110     4.49595   0.24871265          123.7160
# weaptype111     0.66148   0.02299465           18.9764
# weaptype112     6.33593   0.57488838          154.1015

#' Test overall significance of region
wald.test(b=coef(mod.logit), Sigma=vcov(mod.logit), Terms=3:14)
# Wald test:
#   ----------
#   
#   Chi-squared test:
#   X2 = 568.7, df = 12, P(> X2) = 0.0

#' Test overall significance of attacktype1
wald.test(b=coef(mod.logit), Sigma=vcov(mod.logit), Terms=3:14)
# Wald test:
#   ----------
#   
#   Chi-squared test:
#   X2 = 568.0, df = 8, P(> X2) = 0.0

#' Test overall significance of weapontype1
wald.test(b=coef(mod.logit), Sigma=vcov(mod.logit), Terms=25:32)
# Wald test:
#   ----------
#   
#   Chi-squared test:
#   X2 = 201.3, df = 8, P(> X2) = 0.0

with(mod.logit, null.deviance - deviance)
with(mod.logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail=FALSE))
with(mod.logit, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail=FALSE))

anova(mod.logit, test="Chi")

table(data.train$y1.nkill.gt0>0, fitted(mod.logit)>0.5)

# bottom ------------------------------------------------------------------

#' Save the data
# saveRDS(data, "data.rds")