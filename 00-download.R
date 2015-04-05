# install.packages("downloader")
library(downloader)

url <- "http://apps.start.umd.edu/gtd/downloads/dataset/GTD_0814dist.zip"
download(url, dest="./data/GTD_0814dist.zip", mode="wb")
unzip("./data/GTD_0814dist.zip", exdir="./data")
# list.files("./data")