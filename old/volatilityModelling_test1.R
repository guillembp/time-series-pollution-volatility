library(dplyr)
library(data.table)
library(fGarch)
library(qrmtools)
library(rugarch)

setwd("/Users/guillembp/Desktop/Financial Econometrics/Project")

df <- read.csv(file = "/Users/guillembp/Desktop/Financial Econometrics/Project/data/pollution_us_2000_2016.csv", header = TRUE)

df <- df[df$City == "New York",]

df <- df[df$Site.Num == 83 | df$Site.Num == 133,]

df <- df[df$Date.Local %like% 2014 |df$Date.Local %like% 2015 |df$Date.Local %like% 2016,]


plot(df$CO.Mean, type = "l", xlab = df$Date.Local )



