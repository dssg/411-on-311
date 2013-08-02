rm(list = ls())

set.seed(66)

setwd("C:/Users/Zach/Documents/UrbanCCD/Streetlights")

#Load Necessary Packages
library(doBy)
library(reshape)

#Open Crime Portal Data
Crime <- read.csv(file="All_Crimes__April_2012-July_2013.csv", head=TRUE)
crime.type.tab <- as.data.frame(summary(Crime$Primary.Type))

#Open Outages Data from CDOT
Outages <- read.csv(file="Final Outcome Electrical Operations.csv", head=TRUE)
outages.type.tab <- as.data.frame(summary(Outages$Type))
