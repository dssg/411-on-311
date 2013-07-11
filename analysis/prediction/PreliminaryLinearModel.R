rm(list = ls())
setwd("C:/Users/Zach/Documents/UrbanCCD/PredictiveModel/")

#Load Necessary Packages
library(doBy)

#Open ACS Data
ACS_2007_11 <- read.csv(file="ACS_2007_11_Tract_KeyVars.csv", head=TRUE)

#Open Calls Data
Calls_Tract <- read.csv(file="tracts_2009_countsbyday.csv", head=FALSE) 

#Keep Only Potholes
Potholes <- Calls_Tract[Calls_Tract$V3 == "PHF",]

#Remove Huge Dataset from Workspace
rm(Calls_Tract)

#Aggregate Pothole Calls by Month and Tract
Potholes$month <- as.numeric(substr(Potholes$V2, 6, 7))
Potholes$year <- as.numeric(substr(Potholes$V2, 1, 4))

#Aggregate Data by Census Tract, Month, Year
MonthlyData <- summaryBy(V5 ~ V1 + month + year, FUN = c(sum), Potholes)
rm(Potholes)

#Create time variable
MonthlyData$time = 12*(MonthlyData$year - 2009) + MonthlyData$month

#Create Y matrix
Y <- cast(MonthlyData, V1 ~ time, value = 'V5.sum')


#Merge Y and ACS to determine the tracts in common
Y$GEO.id2 <- Y$V1
Merge <- merge(ACS_2007_11, Y, by=c("GEO.id2"))
Merge <- Merge[Merge$Total_Pop!=0,]  #Remove 4 rows with 0 population

#Pull Y back out of Merge to get the common tracts
Y <- Merge[,c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46")]

#Change NA's in Y matrix to 0's
Y[is.na(Y)] <- 0

#Create Lagged Y
Y_lag1 <- Y[,1:45]

#Exclude 1st Time Period for Model
Y_formodel <- Y[,2:46]

#Code additional ACS Variables
Merge$PctElderly <- 100*Merge$Age_65_older/Merge$Total_Pop
Merge$PctBlack <- 100*Merge$Black_NonHisp/Merge$Total_Pop
Merge$PctHisp <- 100*Merge$Hisp/Merge$Total_Pop

#Choose X's for Model
x1 <- Merge$Total_Pop - mean(Merge$Total_Pop)
x2 <- Merge$PctElderly - mean(Merge$PctElderly)
x3 <- Merge$PctBlack - mean(Merge$PctBlack)
x4 <- Merge$PctHisp - mean(Merge$PctHisp)
x5 <- as.numeric(Merge$Pct_Fam_Below_PovLine) - mean(as.numeric(Merge$Pct_Fam_Below_PovLine))
x6 <- as.numeric(Merge$Unemp_Rate) - mean(as.numeric(Merge$Unemp_Rate))

#Monthly Indicator Variables to Capture Seasonality
s_feb <- rep(c(1,0,0,0,0,0,0,0,0,0,0,0),4)[1:45]
s_mar <- rep(c(0,1,0,0,0,0,0,0,0,0,0,0),4)[1:45]
s_apr <- rep(c(0,0,1,0,0,0,0,0,0,0,0,0),4)[1:45]
s_may <- rep(c(0,0,0,1,0,0,0,0,0,0,0,0),4)[1:45]
s_jun <- rep(c(0,0,0,0,1,0,0,0,0,0,0,0),4)[1:45]
s_jul <- rep(c(0,0,0,0,0,1,0,0,0,0,0,0),4)[1:45]
s_aug <- rep(c(0,0,0,0,0,0,1,0,0,0,0,0),4)[1:45]
s_sep <- rep(c(0,0,0,0,0,0,0,1,0,0,0,0),4)[1:45]
s_oct <- rep(c(0,0,0,0,0,0,0,0,1,0,0,0),4)[1:45]
s_nov <- rep(c(0,0,0,0,0,0,0,0,0,1,0,0),4)[1:45]
s_dec <- rep(c(0,0,0,0,0,0,0,0,0,0,1,0),4)[1:45]
s_jan <- rep(c(0,0,0,0,0,0,0,0,0,0,0,1),4)[1:45]


#Set up Data for Linear Model
counts <- c(t(Y_formodel))
counts_lag1 <- c(t(Y_lag1))

s_feb  <- rep(s_feb,852)
s_mar  <- rep(s_mar,852)
s_apr  <- rep(s_apr,852)
s_may  <- rep(s_may,852)
s_jun  <- rep(s_jun,852)
s_jul  <- rep(s_jul,852)
s_aug  <- rep(s_aug,852)
s_sep  <- rep(s_sep,852)
s_oct  <- rep(s_oct,852)
s_nov  <- rep(s_nov,852)
s_dec  <- rep(s_dec,852)
s_jan  <- rep(s_jan,852)

x1 <- rep(x1, each=45)
x2 <- rep(x2, each=45)
x3 <- rep(x3, each=45)
x4 <- rep(x4, each=45)
x5 <- rep(x5, each=45)
x6 <- rep(x6, each=45)

Data <- as.data.frame(cbind(counts, counts_lag1, s_feb, s_mar, s_apr, s_may, s_jun, s_jul, s_aug, s_sep, s_oct, s_nov, s_dec, s_jan, x1, x2, x3, x4, x5, x6))

#Linear Model
Model <- lm(counts ~ counts_lag1+ s_feb + s_mar + s_apr + s_may + s_jun + s_jul + s_aug + s_sep + s_oct + s_nov + s_dec + x1 + x2 + x3, data=Data)

#Model Summary
summary(Model)

#Seasonality Plot
plot(c(Model$coefficients[1],as.vector(Model$coefficients[3:13])+rep(as.vector(Model$coefficients[1]),11)),type="h")

#Residuals over time for 1st Census Tract
plot(as.vector(Model$resid[1:45]),type="l")







