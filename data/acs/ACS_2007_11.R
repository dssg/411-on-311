rm(list = ls())
setwd("C:/Users/Zach/Documents/UrbanCCD/ACS_2007_11")

#READ IN DATA and add Labels

DP02 <- read.csv(file="ACS_11_5YR_DP02_with_ann.csv", head=TRUE, sep=",")
DP02_label <- data.frame(t(read.csv(file="ACS_11_5YR_DP02_metadata.csv", head=FALSE, sep=",")))
names(DP02_label) <- names(DP02)
DP02_w_lab <- rbind(DP02,DP02_label[2,])

DP03 <- read.csv(file="ACS_11_5YR_DP03_with_ann.csv", head=TRUE, sep=",")
DP03_label <- data.frame(t(read.csv(file="ACS_11_5YR_DP03_metadata.csv", head=FALSE, sep=",")))
names(DP03_label) <- names(DP03)
DP03_w_lab <- rbind(DP03,DP03_label[2,])

DP04 <- read.csv(file="ACS_11_5YR_DP04_with_ann.csv", head=TRUE, sep=",")
DP04_label <- data.frame(t(read.csv(file="ACS_11_5YR_DP04_metadata.csv", head=FALSE, sep=",")))
names(DP04_label) <- names(DP04)
DP04_w_lab <- rbind(DP04,DP04_label[2,])

DP05 <- read.csv(file="ACS_11_5YR_DP05_with_ann.csv", head=TRUE, sep=",")
DP05_label <- data.frame(t(read.csv(file="ACS_11_5YR_DP05_metadata.csv", head=FALSE, sep=",")))
names(DP05_label) <- names(DP05)
DP05_w_lab <- rbind(DP05,DP05_label[2,])


#Merge 4 datasets together based on geography
#Note Labels are saved in last row of data file

m1 <- merge(DP02_w_lab, DP03_w_lab, by=c("GEO.id","GEO.id2","GEO.display.label"))
m2 <- merge(m1, DP04_w_lab, by=c("GEO.id","GEO.id2","GEO.display.label"))
Data_Tract <- merge(m2, DP05_w_lab, by=c("GEO.id","GEO.id2","GEO.display.label"))
rm(list=(ls()[ls()!="Data_Tract"]))
save(Data_Tract, file="Data_Tract")



