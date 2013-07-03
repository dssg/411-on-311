rm(list = ls())
setwd("C:/Users/Zach/Documents/UrbanCCD/ACS_2007_11")

#READ IN DATA; add column name suffixes for merging
DP02 <- read.csv(file="ACS_11_5YR_DP02_with_ann.csv", head=TRUE, sep=",")
ncol <- ncol(DP02)
colnames(DP02)[4:ncol] <- paste(colnames(DP02)[4:ncol], "_DP02", sep="")

DP03 <- read.csv(file="ACS_11_5YR_DP03_with_ann.csv", head=TRUE, sep=",")
ncol <- ncol(DP03)
colnames(DP03)[4:ncol] <- paste(colnames(DP03)[4:ncol], "_DP03", sep="")

DP04 <- read.csv(file="ACS_11_5YR_DP04_with_ann.csv", head=TRUE, sep=",")
ncol <- ncol(DP04)
colnames(DP04)[4:ncol] <- paste(colnames(DP04)[4:ncol], "_DP04", sep="")

DP05 <- read.csv(file="ACS_11_5YR_DP05_with_ann.csv", head=TRUE, sep=",")
ncol <- ncol(DP05)
colnames(DP05)[4:ncol] <- paste(colnames(DP05)[4:ncol], "_DP05", sep="")



#Merge 4 datasets together based on geography

m1 <- merge(DP02, DP03, by=c("GEO.id","GEO.id2","GEO.display.label"))
m2 <- merge(m1, DP04, by=c("GEO.id","GEO.id2","GEO.display.label"))
ACS_2007_11_Tract <- merge(m2, DP05, by=c("GEO.id","GEO.id2","GEO.display.label"))
rm(list=(ls()[ls()!="ACS_2007_11_Tract"]))
write.csv(ACS_2007_11_Tract, file="ACS_2007_11_Tract.csv", row.names=FALSE)





