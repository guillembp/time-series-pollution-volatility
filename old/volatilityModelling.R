library(dplyr)
library(data.table)
library(fGarch)



setwd("/Users/guillembp/Desktop/Financial Econometrics/Project")

df <- read.csv(file = "/Users/guillembp/Desktop/Financial Econometrics/Project/data/pollution_us_2000_2016.csv", header = TRUE)

df <- df[df$City == "New York",]

df <- df[df$Address %like% "200TH STREET AND SOUTHERN BLVD",]


