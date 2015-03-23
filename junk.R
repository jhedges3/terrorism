# testName <- "this_is_a_test"
# sub("_","",testName) # this will only remove the first _
# gsub("_","",testName) # this will remove all of them
# gsub("_",".",testName) # replace all _'s with .'s
# 
# Df <- data.frame(
#   x = sample(1:5,30,r=T),
#   y = sample(1:5,30,r=T),
#   z = sample(1:5,30,r=T),
#   w = sample(1:5,30,r=T)
# )
# 
# Df[,c("y","w")] <- as.factor(as.character(unlist(Df[,c("y","w")])))
# 
# # Contingency tables or crosstabs can be produced using either the table( ) or xtabs( ) function. Table is easier, so I'll illustrate that first... 
# # 
# # > with(ucb, table(gender, admitted))   # or table(ucb$gender, ucb$admitted)
# #         admitted
# # gender     no  yes
# #   female 1278  557
# #   male   1493 1198
# 
# xtabs(nkill ~ attacktype1 + targtype1, data=data.06to13.select)
# 
# data.06to13.select %>% summarise_each(funs(mean, sd), nkill, nkillus, nkillter)
# 
# 
# install.packages("caret")
# library(caret)
# 
# 
# x <- data$nkill[1:200]
# v <- c(0, 1, 6, 250) # create two bins [5,10) and [10,15)
# foo <- cbind(x, findInterval(x, v))
# 
# # plot(data.06to13$nkillus, aes(nkillus)) + 
# #   stat_density(aes(nkill), position = "stack", color = "black")
# # 
# # p <- ggplot(data, aes(1, nkill))
# # p + geom_boxplot()
# # 
# # p <- ggplot(mtcars, aes(factor(cyl), mpg))
# # p + geom_boxplot()
# # 
# # ggplot(data, aes(nkill)) + 
# #   stat_density(aes(nkill), position = "stack", color = "black")
# # 
# # ggplot(data, aes(x=nkill)) + geom_density()
# # 
# # plot(data$eventid, cumsum(data$nkill))
# 
# 
# assoc(data.100[,c("iyear","imonth")], shade=TRUE)
# assoc(data[,c("region","iyear")], shade=TRUE)
# 
# ggplot(stack(data.100[, "nkill"]),
#        aes(x=values)) +
#   geom_density(aes(group=ind, colour=ind, fill=ind), alpha=0.5) +
#   geom_rug(sides="b", col="red", alpha=.25) +
#   xlim(0, 300) +
#   labs(x="kill count") +
#   ggtitle('Top 107 incidents from 2006-2013 [03-003-01-002]') +
#   theme(legend.title=element_blank()) +
#   theme(panel.background = element_rect(fill = '#fbf9ea'))
# ggsave("03-003-01-002.pdf",
#        path=path.output,
#        units="in",
#        width=9,
#        height=6.5)
# 
# 
# ggplot(
#   arrange(data.100, region),
#   aes(x=factor(iyear),
#       y=nkill,
#       fill=factor(region))) +
#   geom_bar(stat = "identity")
# 
# ggplot(data.100, aes(x=factor(iyear), y=nkill)) +
#   geom_bar(aes(fill=factor(suicide)), stat="identity")
# 
# ggplot(
#   data.100,
#   aes(
#     x=factor(iyear),
#     y=nkill,
#     fill=factor(targtype1),
#     order=levels(factor(data.100$targtype1))
#   )
# ) +
#   geom_bar(stat = "identity")
# 
# # ggplot(data.100, aes(x=factor(iyear), y=nkill, fill=factor(region))) +
# #   geom_bar(position="identity")
# 
# 
# 
# assoc(snee, shade=TRUE)
# 
# ggplot(stack(data.100[, "nkill"]),
#        aes(x=values)) +
#   geom_density(aes(group=ind, colour=ind, fill=ind), alpha=0.5) +
#   geom_rug(sides="b", col="red", alpha=.25) +
#   xlim(0, 300) +
#   labs(x="kill count") +
#   ggtitle('Top 107 incidents from 2006-2013 [03-003-01-001]') +
#   theme(legend.title=element_blank()) +
#   theme(panel.background = element_rect(fill = '#fbf9ea'))
# 
# 
# ggplot(stack(data.100[, "nkill"]),
#        aes(x=values)) +
#   geom_density(aes(group=ind, colour=ind, fill=ind), alpha=0.5) +
#   geom_rug(sides="b", col="red", alpha=.25) +
#   xlim(0, 300) +
#   labs(x="kill count") +
#   ggtitle('Top 107 incidents from 2006-2013 [03-003-01-001]') +
#   theme(legend.title=element_blank()) +
#   theme(panel.background = element_rect(fill = '#fbf9ea'))
# 
# # f <- function(a) g(a)
# # g <- function(b) h(b)
# # h <- function(c) i(c)
# # i <- function(d) "a" + d
# # f(10)
# 
# foo <- data.100 %>% 
#   group_by(extended)

# codes lookup ------------------------------------------------------------

# MAKING LOOKUP TABLE FOR gtd CODES
path.codes <- "/Users/jameshedges/Documents/Projects/terrorism/terrorism"
files <- list.files(path=path.codes)
bla <- read.csv(paste(path.codes, files[1], sep="/"))
