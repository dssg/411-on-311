rm(list = ls())
setwd("C:/Users/Zach/Documents/UrbanCCD/Census2000")

#READ IN DATA; add column name suffixes for merging
DP1 <- read.csv(file="DEC_00_SF1_DP1_with_ann.csv", head=TRUE, sep=",")
ncol <- ncol(DP1)
colnames(DP1)[4:ncol] <- paste(colnames(DP1)[4:ncol], "_DP1", sep="")

DP2 <- read.csv(file="DEC_00_SF3_DP2_with_ann.csv", head=TRUE, sep=",")
ncol <- ncol(DP2)
colnames(DP2)[4:ncol] <- paste(colnames(DP2)[4:ncol], "_DP2", sep="")

DP3 <- read.csv(file="DEC_00_SF3_DP3_with_ann.csv", head=TRUE, sep=",")
ncol <- ncol(DP3)
colnames(DP3)[4:ncol] <- paste(colnames(DP3)[4:ncol], "_DP3", sep="")

DP4 <- read.csv(file="DEC_00_SF3_DP4_with_ann.csv", head=TRUE, sep=",")
ncol <- ncol(DP4)
colnames(DP4)[4:ncol] <- paste(colnames(DP4)[4:ncol], "_DP4", sep="")


#Merge 4 datasets together based on geography
#Note Labels are saved in last row of data file

m1 <- merge(DP1, DP2, by=c("GEO.id","GEO.id2","GEO.display.label"))
m2 <- merge(m1, DP3, by=c("GEO.id","GEO.id2","GEO.display.label"))
Census2000_Tract <- merge(m2, DP4, by=c("GEO.id","GEO.id2","GEO.display.label"))
rm(list=(ls()[ls()!="Census2000_Tract"]))
write.csv(Census2000_Tract, file="Census2000_Tract.csv", row.names=FALSE)



