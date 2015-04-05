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
# install.packages("aod")
# install.packages("ROCR")

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
library(ROCR)

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
mod.logit.summary <- summary(mod.logit)
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

#' Sorted coefficients
mod.logit.summary.coef <- mod.logit.summary$coefficients
mod.logit.summary.coef[order(mod.logit.summary.coef[,"Estimate"], 
                             decreasing=TRUE),]
#                   Estimate   Std. Error       z value     Pr(>|z|)
# attacktype19  7.2829683579 137.91003072  5.280956e-02 9.578836e-01
# suicide1      6.8355714668   0.72279536  9.457132e+00 3.165037e-21
# (Intercept)   2.9893354706   1.28957684  2.318075e+00 2.044526e-02
# weaptype19    2.3527708671   1.06858462  2.201764e+00 2.768200e-02
# region4       2.2678163198   0.60000932  3.779635e+00 1.570583e-04
# weaptype112   1.8462363900   1.31440694  1.404616e+00 1.601356e-01
# weaptype15    1.7519792154   1.08077825  1.621035e+00 1.050102e-01
# region11      1.6108734512   0.39358642  4.092808e+00 4.261815e-05
# region10      1.5540284847   0.38998676  3.984824e+00 6.753025e-05
# weaptype110   1.5031762247   1.44894133  1.037431e+00 2.995352e-01
# region6       1.0307951504   0.38940660  2.647092e+00 8.118723e-03
# region5       0.6603120692   0.39281497  1.680975e+00 9.276781e-02
# weaptype16    0.6182160868   1.08842401  5.679920e-01 5.700404e-01
# claimed1      0.5596358775   0.05485053  1.020293e+01 1.923888e-24
# region12      0.4309740356   0.40249142  1.070766e+00 2.842748e-01
# region3       0.4240979631   0.40750440  1.040720e+00 2.980055e-01
# region13     -0.0008905702   1.35025127 -6.595589e-04 9.994737e-01
# weaptype18   -0.0053510358   1.09067044 -4.906189e-03 9.960854e-01
# region2      -0.2384155769   0.74279312 -3.209717e-01 7.482318e-01
# extended1    -0.3969452342   0.13689705 -2.899589e+00 3.736518e-03
# weaptype111  -0.4132707776   1.50971353 -2.737412e-01 7.842835e-01
# region9      -0.7592268393   0.62089229 -1.222800e+00 2.214054e-01
# region7      -1.2728096269   1.08731700 -1.170597e+00 2.417609e-01
# region8      -1.6558524480   0.46676272 -3.547525e+00 3.888688e-04
# attacktype12 -4.3839029682   0.58341248 -7.514243e+00 5.724131e-14
# attacktype13 -5.3567242918   0.58919352 -9.091621e+00 9.757317e-20
# attacktype16 -5.5900922418   0.59605001 -9.378562e+00 6.687830e-21
# attacktype15 -6.5183116674   0.77656025 -8.393826e+00 4.705722e-17
# weaptype17   -6.8057854851 196.97163353 -3.455211e-02 9.724369e-01
# attacktype17 -7.1924838135   0.60433826 -1.190142e+01 1.163494e-32
# attacktype18 -7.3085720943   0.67499665 -1.082757e+01 2.548338e-27
# attacktype14 -8.1805775435   0.85495250 -9.568458e+00 1.085115e-21
#' (1) suicide has a very big impact on log odds, with 6.82 value
#' (2) attacktype1 and weaptype19 seem suspect and the prior esp
#'  consider removing them and redoing, or removing some levels or something
#' (3) regions 4,10,11,6 all showing significant positive effects
#' (4) claimed has a small, but positive, sig. B
#' (5) region8 has significant negative effect of -1.6
#' (6) if they are real attack types 2,3,6,5,7,8,4 all have negative effects
#' (7) weapon type 7 also has sig. neg. B; this effect has very large error

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
#                      OR        2.5 %            97.5 %
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
# attacktype19 1455.30148   0.00000069                NA -seems error; low data
# claimed1        1.75004   1.57200131            1.9491
# weaptype15      5.76600   1.01835220          109.3549
# weaptype16      1.85561   0.32074356           35.4727
# weaptype17      0.00111           NA 529705868740.7859 -seems error; low data
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


# TEST --------------------------------------------------------------------

probs.test <- predict(mod.logit, data.test, type="response")
#' Version 1
y1.nkill.gt0.est <- rep(0, length(probs.test))
y1.nkill.gt0.est[probs.test >= 0.5] <- 1
table(data.test$y1.nkill.gt0, y1.nkill.gt0.est)
#' Version 2
y1.nkill.gt0.est.v2 <- cut(probs.test, breaks=c(-Inf, 0.5, Inf), labels=c(0,1))
table(data.test$y1.nkill.gt0, y1.nkill.gt0.est.v2)
#' Confusion matrix
confusionMatrix(data.test$y1.nkill.gt0, y1.nkill.gt0.est.v2)
#' ROC curve
pred.fit = prediction(probs.test, data.test$y1.nkill.gt0)
perf.fit = performance(pred.fit, "tpr", "fpr")
plot(perf.fit,
     col="blue",
     lwd=2,
     main="ROC curve: logit model on y1 (nkill > 0)")
abline(a=0, b=1, lwd=2, lty=2, col="gray")

# FIT WITH CARET ----------------------------------------------------------

mod.logit.v2 = train(y1.nkill.gt0 ~ extended + region + suicide + attacktype1 + claimed + weaptype1,
                     method="glm",
                     data=data.train,
                     family=binomial(link='logit'))
mod.logit.v2.summary <- summary(mod.logit.v2)
# Call:
#   NULL
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -3.6446  -0.8912   0.0511   0.7855   3.2587  
# 
# Coefficients: (3 not defined because of singularities)
# Estimate Std. Error z value Pr(>|z|)    
# (Intercept)   5.395e+00  1.071e+00   5.039 4.68e-07 ***
#   extended1    -3.969e-01  1.369e-01  -2.900 0.003737 ** 
#   region2      -2.384e-01  7.428e-01  -0.321 0.748232    
# region3       4.241e-01  4.075e-01   1.041 0.298006    
# region4       2.268e+00  6.000e-01   3.780 0.000157 ***
#   region5       6.603e-01  3.928e-01   1.681 0.092768 .  
# region6       1.031e+00  3.894e-01   2.647 0.008119 ** 
#   region7      -1.273e+00  1.087e+00  -1.171 0.241761    
# region8      -1.656e+00  4.668e-01  -3.548 0.000389 ***
#   region9      -7.592e-01  6.209e-01  -1.223 0.221405    
# region10      1.554e+00  3.900e-01   3.985 6.75e-05 ***
#   region11      1.611e+00  3.936e-01   4.093 4.26e-05 ***
#   region12      4.310e-01  4.025e-01   1.071 0.284275    
# region13     -8.906e-04  1.350e+00  -0.001 0.999474    
# suicide1      6.836e+00  7.228e-01   9.457  < 2e-16 ***
#   attacktype12 -4.384e+00  5.834e-01  -7.514 5.72e-14 ***
#   attacktype13 -5.357e+00  5.892e-01  -9.092  < 2e-16 ***
#   attacktype14 -8.181e+00  8.550e-01  -9.568  < 2e-16 ***
#   attacktype15 -6.518e+00  7.766e-01  -8.394  < 2e-16 ***
#   attacktype16 -5.590e+00  5.960e-01  -9.379  < 2e-16 ***
#   attacktype17 -7.192e+00  6.043e-01 -11.901  < 2e-16 ***
#   attacktype18 -7.309e+00  6.750e-01 -10.828  < 2e-16 ***
#   attacktype19  7.283e+00  1.379e+02   0.053 0.957884    
# claimed0     -5.596e-01  5.485e-02 -10.203  < 2e-16 ***
#   claimed1             NA         NA      NA       NA    
# weaptype12   -1.846e+00  1.314e+00  -1.405 0.160136    
# weaptype15   -9.426e-02  8.076e-01  -0.117 0.907084    
# weaptype16   -1.228e+00  8.184e-01  -1.501 0.133466    
# weaptype17   -8.652e+00  1.970e+02  -0.044 0.964964    
# weaptype18   -1.852e+00  8.193e-01  -2.260 0.023821 *  
#   weaptype19    5.065e-01  8.100e-01   0.625 0.531755    
# weaptype110  -3.431e-01  1.280e+00  -0.268 0.788681    
# weaptype111  -2.260e+00  1.326e+00  -1.703 0.088487 .  
# weaptype112          NA         NA      NA       NA    
# weaptype113          NA         NA      NA       NA    
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

# probs.test.v2 <- predict(mod.logit.v2, data.test)
# y1.nkill.gt0.est.vX <- cut(probs.test.v2, breaks=c(-Inf, 0.5, Inf), labels=c(0,1))
# confusionMatrix(data.test$y1.nkill.gt0, y1.nkill.gt0.est.vX)


# bottom ------------------------------------------------------------------

#' Save the data
# saveRDS(data, "data.rds")