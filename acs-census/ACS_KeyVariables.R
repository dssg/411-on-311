##  Organizes key variables from ACS 2007-11 ACS for Chicago census tracts

rm(list = ls())
setwd("C:/Users/Zach/Documents/UrbanCCD/ACS_2007_11")

#Open ACS_2007_11_Tract
ACS_2007_11_Tract <- read.csv(file="C:/Users/Zach/Documents/UrbanCCD/ACS_2007_11/ACS_2007_11_Tract.csv", head=TRUE, sep=",")

#Create variables
ACS_2007_11_Tract$Total_Pop <- ACS_2007_11_Tract$HC01_VC03_DP05
ACS_2007_11_Tract$Male <- ACS_2007_11_Tract$HC01_VC04_DP05
ACS_2007_11_Tract$Female <- ACS_2007_11_Tract$HC01_VC05_DP05
ACS_2007_11_Tract$Age_0_14 <- ACS_2007_11_Tract$HC01_VC07_DP05 + ACS_2007_11_Tract$HC01_VC08_DP05 + ACS_2007_11_Tract$HC01_VC09_DP05
ACS_2007_11_Tract$Age_15_24 <- ACS_2007_11_Tract$HC01_VC10_DP05 + ACS_2007_11_Tract$HC01_VC11_DP05
ACS_2007_11_Tract$Age_25_64 <- ACS_2007_11_Tract$HC01_VC12_DP05 + ACS_2007_11_Tract$HC01_VC13_DP05 + ACS_2007_11_Tract$HC01_VC14_DP05 + ACS_2007_11_Tract$HC01_VC15_DP05 + ACS_2007_11_Tract$HC01_VC16_DP05
ACS_2007_11_Tract$Age_65_older <- ACS_2007_11_Tract$HC01_VC17_DP05 + ACS_2007_11_Tract$HC01_VC18_DP05 + ACS_2007_11_Tract$HC01_VC19_DP05
ACS_2007_11_Tract$White_NonHisp <- ACS_2007_11_Tract$HC01_VC88_DP05
ACS_2007_11_Tract$Black_NonHisp <- ACS_2007_11_Tract$HC01_VC89_DP05
ACS_2007_11_Tract$Asian <- ACS_2007_11_Tract$HC01_VC91_DP05
ACS_2007_11_Tract$Hisp <- ACS_2007_11_Tract$HC01_VC82_DP05
ACS_2007_11_Tract$OtherRace <- ACS_2007_11_Tract$HC01_VC03_DP05 - ACS_2007_11_Tract$HC01_VC88_DP05 - ACS_2007_11_Tract$HC01_VC89_DP05 - ACS_2007_11_Tract$HC01_VC91_DP05 - ACS_2007_11_Tract$HC01_VC82_DP05
ACS_2007_11_Tract$Median_Income_and_Benefits <- ACS_2007_11_Tract$HC01_VC85_DP03
ACS_2007_11_Tract$Pct_Fam_Below_PovLine <- ACS_2007_11_Tract$HC03_VC156_DP03
ACS_2007_11_Tract$Unemp_Rate <- ACS_2007_11_Tract$HC03_VC08_DP03

#Drop extra variables (Note: ACS_2007_11_Tract has 2035 - 3 extra variables)
ncol <- ncol(ACS_2007_11_Tract)
ACS_2007_11_Tract <- ACS_2007_11_Tract[,c(1:3,2036:ncol)]

#Export as csv
write.csv(ACS_2007_11_Tract, file="ACS_2007_11_Tract_KeyVars.csv")











