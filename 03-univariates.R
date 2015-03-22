# install.packages("dplyr")
# install.packages("e1071")
# install.packages("pastecs")
# install.packages("ggplot2")
# install.packages("lubridate")
# install.packages("arules")
# install.packages("MASS")
# install.packages("vcd")
# install.packages("prettyR")
library(dplyr)
library(e1071)
library(pastecs)
library(ggplot2)
library(lubridate)
library(arules)
library(MASS)
library(vcd)
library(prettyR)

#' Get the selected data, saved in 02-selection.R
data <- readRDS("data.06to13.select.rds")

#' Set output path
path.output <- "/Users/jameshedges/Documents/Projects/terrorism/output"

# # 03-001 - summary stats on everything ------------------------------------
# 
# summary(data)
# #' (1) mean year is 2011, slightly forward from center, but close
# #' (2) mongth is also sligthly forward shifted, with lower levels around 1st and
# #' 2nd months of the year
# #' (3) most incidents were not extended, only ab 900 out of ab 29k
# 
# #' (4) top five countries:
# #' [1] (95) Iraq with 7.5K;
# #' [2] (153) Pakistan with 5K;
# #' [3] (4) Afghanistan with 3.2K;
# #' [4] (92) India with 2.7K;
# #' [5] (205) Thailand with 1.3K.
# #' (5) top two regions:
# #' [1] (6) South Asia with 11.8K;
# #' [2] (10) Middle East and North Africa 10K;
# 
# #' (6) most are not part of multiple incidents, ab 4K vs. 25K
# #' (7) most are not suicide 1.7K are vs. 27.4K are not
# #' (8) top five attack types:
# #' [1] (3) Kidnapping with 17.2K
# #' [2] (2) Hijacking with 7.6K
# #' [3] (7) Armed Assault with 1.7K
# #' [4] (1) Assassination with 1.2K
# #' [5] (6) Unkown with 1K
# 
# #' (9) top four target types:
# #' [1] (14) Private Citizens & Property with 8.5K
# #' [2] (3) Police with 5.4K
# #' [3] (2) Government with 4.2K
# #' [4] (1) Business with 2.9K
# 
# #' (10) top four nationality of target/victim:
# #' [1] (95) Iraq with 7.4K
# #' [2] (153) Pakistan with 4.9K
# #' [3] (4) Afghanistan with 3.1K
# #' [4] (92) India with 2.7K
# #' (11) these are mirroring the country of the attacks
# 
# #' (12) number of perpretrators is unkown in many cases; will need to deal with
# #' this separately
# 
# #' (13) top three weapon types:
# #' [1] (6) Explosives/Bombs/Dynamite with 17.8K
# #' [2] (5) Firearms with 9K
# #' [3] (8) Incendiary with 1.6K
# 
# #' (14) number of kills is 2.03 mean, with positive skew, and max of 250
# #' (15) number of US kills is 0.0052 mean, with positive skew, and max of 13
# #' (16) number of terrorist kills is 0.18 mean, with positive skew, and max of
# #' 70
# 
# # 03-002 summary of kills -------------------------------------------------
# 
# #' Response variables are numeric values of total kilss, US kills, and terrorist
# #' kills
# col.kills <- c("nkill", "nkillus", "nkillter")
# 
# #' Round kills
# data[, col.kills] <- sapply(data[, col.kills], round)
# 
# #' Summary statistics of output variables
# options(scipen=100)
# options(digits=2)
# stat.desc(data[, col.kills])
# #' lots of important information here
# #' (1) number of values is 29.2K, so that's current number of incidents
# #' (2) null counts for the three: 13.5K, 29.1K, 26.6K
# #' suprised by how many 0 kill incidents there are for the terrorists
# #' many incidents don't result in any kills even tho they're considered
# #' a success
# #' nearly all incidents do not result in kills of US citizens
# #' (3) max values for the three 250, 13, and 70
# #' these seem reasonable (not too high), might be much lower than
# #' people expect
# #' (4) sum is 59.1K for all, 151 for US, and 5.3K for terrorist kills
# #' these seem very low, especially for US43
# #' number of US deaths in motor vehicle accidents
# #' in roughly the same period 298K
# #' (5) mean number of kills per incident: 2.027, 0.005, 0.18
# #' (6) standard deviations are quite large, 5.8, 0.16, 1.18
# #' because of extended right tail
# 
# #' Skew and kurtosis of kills variables
# fun.skew.kurt <- function(x) {
#   c(
#     skewness = skewness(x), 
#     kurtosis = kurtosis(x)
#   )
# }
# sapply(
#   data[, col.kills],
#   fun.skew.kurt)
# #' (1) very clear that these distributions are not symmetrical
# #' (2) value of skewness is quite large, and especially large for nkillsus
# #' the distribution for number of US kills is especially asymmetrical, with
# #' skewness value of 54
# #' (3) these distributions are very clearly not Gaussian
# 
# #' Density plot of three kill measures
# #' (1) http://www.color-hex.com/color/fbf9ea
# ggplot(stack(data[, col.kills]),
#        aes(x=values)) +
#   geom_density(aes(group=ind, colour=ind, fill=ind), alpha=0.5) +
#   xlim(0, 10.5) +
#   labs(x="kill count") +
#   ggtitle('All incidents from 2006-2013 [03-002-02-001]') +
#   theme(legend.title=element_blank()) +
#   theme(panel.background = element_rect(fill = '#fbf9ea'))
# ggsave("03-002-02-001.pdf",
#        path=path.output,
#        units="in",
#        width=5,
#        height=6.5)
# #' (1) shows what we expect in terms very low US kills
# #' and quite a bit higher for other measures of overall kills
# #' and terrorist kills
# #' (2) shape is very highly skewed, resulting in part from high
# #' proportion of 0-kill incidents
# #' (3) values are quantized of course (can't have fractions of kills)
# #' (4) there are slightly higher overall total kills than kills of terrorists
# #' and most of the incidents fall within the values of 0-5 or even 0-2
# #' (5) probably helpful to make logical inds on these
# #' (ai) 0
# #' (aii) >0
# #' (bi) 0
# #' (bii) 1-5
# #' (biii) >5
# 
# #' Relationship between dates and kills
# dates <- as.Date(
#   paste(
#     as.character(data$iyear),
#     as.character(data$imonth),
#     as.character(data$iday),
#     sep="/"),
#   format="%Y/%m/%d"
# )
# data <- mutate(data, dates=dates)
# data.tmp <- data[data$iday!=0,]
# data.tmp %>%
#   group_by(wday(data.tmp$dates)) %>%
#   summarise(avg = mean(nkill)) %>%
#   arrange(avg)
# #' (1) seems like there is a relationship between kills and day of the week
# #' days towards the end of the week have higher kills, with highest
# #' usually coming on Saturday, followed by Friday
# #' (2) effect probably applies for other measures as well,
# #' such as for US kills and terrorist kills
# #' (3) not clear if there are different attack types or weapon types going
# #' with date, is the effect related to there being more attacks on these days
# #' is reverse true that there are more 0-kill days for early days in the week
# 
# #' Kills indexing
# data <- mutate(data,
#                ind.nkill=findInterval(data$nkill, c(0, 1, 6, 251)))
# #' (1) needed to check this quite a bit and adjust; important to consider
# #' that the data were rounded before this
# #' (2)
# #' 0 kill incidents -> ind=1
# #' 1-5 kill incidents -> ind=2
# #' >5 kill incidents -> ind=3
# #' (3) counts of these match with above
# #' 13.5K ind=1 (roughly 45% 0-kill incidents)
# #' 13.2K ind=2 (roughly 45% 1-5kill incidents)
# #' 2.4K ind=3 (roughly 8% >5kill incidents)
# 
# # 03-003 top 100 incidents ------------------------------------------------
# 
#' Top 107 incidents in terms number of kills (includes ties)
data.100 <- top_n(data, 100, nkill)
# 
# summary(data.100)
# #' (1) 10 of the 107 incidents beyond 24 hours; not sure what the ratio for this would be for the entire dataset, but it could be an interesting thing to check; probably wouldn't be that surprising that this would be predictive of kills
# #' (2) top countries: Iraq (95) with 37 incidents; Pakistan (153) with 31 incidents; and Afghanistan (4) with 7; not surprising and basically reflects what we see in the dataset overall
# #' (3) top regions: Middle East & North Africa (10) with 49; South Asia (6) with 41; and Sub-Saharan Africa (11) with 15. Note that there is 1 incident for Western Europ and 0 incidents for North America.
# #' (4) consider country and region has being colinear, with one derived from the other; what's the right thing to do there at the point of the model; is one more helpful in terms of prediction? which should we let go of? how will these show up as such?
# #' (5) another interesting variable in multiple; categorical that gives whether the incident was part of multiple incidents or not; here ~1:10 is
# #' (6) ~63% of these are suicide attacks; percentage is much higher than for the entire set of incidents; this seems like it will be highly predictive
# #' (7) attack types are nearly all bombing/explosion, ~77 percent; could make feature out of suicide bombings; also note that this variable may be highly colinear with weaptype1 (type of weapon)
# #' (8) 47% on private citizens and property, these are public areas including busy intersections, etc.; doesn't include restaurants and movie theaters, which are counted as business; roughly equal amounts in 15,2,1,3: religious figures/institutions; government (general); business; and police
# #' (9) nationality of targets is primarily Iraqi or Pakistani
# #' (10) number of perpetrators is problematic considering it is often not recorded, 38 out of 107 are not recorded; 23.8K of these in the overall dataset; what to do with them; could either do the models without it, or do the models on just those with it, or both
# #' (11) much higher proportion are claimed than in the overall dataset; ~42% are claimed in the top 100, vs 12.6% in the overall dataset
# #' (12) 81% of these are explosives/bombs attacks; next nearest is firearm attacks with ~15%

#' Plot distribution of kills for these incidents
ggplot(stack(data.100[, "nkill"]),
       aes(x=values)) +
  geom_density(aes(group=ind, colour=ind, fill=ind), alpha=0.5) +
  geom_rug(sides="b", col="red", alpha=.25) +
  xlim(0, 300) +
  labs(x="kill count") +
  ggtitle('Top 107 incidents from 2006-2013 [03-003-01-001]') +
  theme(legend.title=element_blank()) +
  theme(panel.background = element_rect(fill = '#fbf9ea'))
ggsave("03-003-01-001.pdf",
       path=path.output,
       units="in",
       width=5,
       height=6.5)

#' Table of years
table(data.100$iyear)
#' (1) curious property of 2006 that only 1 incident in this set from there
#' (2) even and high representation 2007-2011, then a down year and back up higher

#' Plots of year by kills for top 107 and colored by a particular variable
fig.name.pre <- "03-003-03"
vars <- c(
  "extended",
  "country",
  "region",
  "multiple",
  "suicide",
  "attacktype1",
  "targtype1",
  "natlty1",
  "claimed",
  "weaptype1"
)
for (i in vars) {
  print(ggplot(arrange_(data.100, i),
               aes(x=factor(iyear), y=nkill, fill=get(i))) +
          geom_bar(stat="identity") +
          labs(x="year", y="kills") +
          ggtitle('top 107 incidents from 2006-2013 by kills') +
          theme(panel.background=element_rect(fill='#fbf9ea')))
  fig.name <- paste(paste(fig.name.pre, toupper(i), sep="-"), "pdf", sep=".")
  ggsave(fig.name, path=path.output, units="in", width=5, height=6.5)
}