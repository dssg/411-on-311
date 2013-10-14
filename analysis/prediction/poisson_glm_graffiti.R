rm(list = ls())

set.seed(66)

#Load Necessary Packages
library(R2jags)
library(coda)
library(doBy)
library(reshape)
library(glmmML)
#library(BaylorEdPsych)

#Open ACS Data
ACS_2007_11 <- read.csv(file="../../data/acs_2007_11_tract_variables.csv", head=TRUE)

#Open Calls Data
Calls_Tract <- read.csv(file="data/tracts_2009_countsbyday.csv", head=FALSE) 

#Open Tract Neighbors Indicator File (for CAR covariance matrix)
Tract_Neighbors <- read.csv(file="data/Tract Neighbors.csv", head=TRUE)

#List of Unique Tracts
TractList <- as.data.frame(unique(Tract_Neighbors$src_GEOID1))

#Keep Only Graffiti
Graffiti <- Calls_Tract[Calls_Tract$V3 == "GRAF",]
#Graffiti <- Calls_Tract[Calls_Tract$V3 == "PHF",]

#Remove Huge Dataset from Workspace
rm(Calls_Tract)

#Create Month and Year Variables
Graffiti$month <- as.numeric(substr(Graffiti$V2, 6, 7))
Graffiti$year <- as.numeric(substr(Graffiti$V2, 1, 4))

#Aggregate Data by Census Tract, Month, Year
MonthlyData <- summaryBy(V5 ~ V1 + month + year, FUN = c(sum), Graffiti)
rm(Graffiti)

#Create time variable
MonthlyData$time = 12*(MonthlyData$year - 2009) + MonthlyData$month

#Create Y matrix
Y <- cast(MonthlyData, V1 ~ time, value = 'V5.sum')

#Exclude last time period for being a partial month
Y <- Y[,1:dim(Y)[2]-1]

#Merge Y and ACS to determine the tracts in common
Y$GEO.id2 <- Y$V1
PreMerge1 <- merge(ACS_2007_11, Y, by=c("GEO.id2"))
PreMerge1 <- PreMerge1[PreMerge1$Total_Pop!=0,]  #Remove 4 rows with 0 population

# Now Merge "PreMerge1" and TractList to find tracts in common
TractList$GEO.id2 <- TractList[,1]
PreMerge2 <- merge(PreMerge1, TractList, by=c("GEO.id2"))
Merge <- merge(TractList, PreMerge2, by=c("GEO.id2"))

#Pull Y back out of Merge to get the common tracts
Y <- Merge[,c( "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46")]

#Change NA's in Y matrix to 0's
Y[is.na(Y)] <- 0

#Create Lagged Y
Y_lag2 <- Y[,1:44]
Y_lag1 <- Y[,2:45]

#Exclude 1st 2 Time Periods for Model
Y <- Y[,3:46]

#Code additional ACS Variables
Merge$FracElderly <- Merge$Age_65_older/Merge$Total_Pop
Merge$FracBlack <- Merge$Black_NonHisp/Merge$Total_Pop
Merge$FracHisp <- Merge$Hisp/Merge$Total_Pop
Merge$FracPovLine <- as.numeric(as.character(Merge$Pct_Fam_Below_PovLine))/100

#Choose X's for Model; Make All Demeaned for Interpretability of Intercept!
x1 <- Merge$Total_Pop/1000 - mean(Merge$Total_Pop/1000)  #Demeaned Population in 1,000s
x2 <- Merge$FracElderly - mean(Merge$FracElderly)
x3 <- Merge$FracBlack - mean(Merge$FracBlack)
x4 <- Merge$FracHisp - mean(Merge$FracHisp)
x5 <- as.numeric(Merge$FracPovLine) - mean(as.numeric(Merge$FracPovLine))
x6 <- as.numeric(as.character(Merge$Unemp_Rate)) - mean(as.numeric(as.character(Merge$Unemp_Rate))) 
x7 <- as.numeric(as.character(Merge$Median_Income_and_Benefits))/1000 - mean(as.numeric(as.character(Merge$Median_Income_and_Benefits))/1000) #Median Income in 1000s

#Monthly Indicator Variables to Capture Seasonality
s1 <- rep(c(0,0,0,0,0,0,0,0,0,0,1,0),4)[1:44]
s2 <- rep(c(0,0,0,0,0,0,0,0,0,0,0,1),4)[1:44]
s3 <- rep(c(1,0,0,0,0,0,0,0,0,0,0,0),4)[1:44]
s4 <- rep(c(0,1,0,0,0,0,0,0,0,0,0,0),4)[1:44]
s5 <- rep(c(0,0,1,0,0,0,0,0,0,0,0,0),4)[1:44]
s6 <- rep(c(0,0,0,1,0,0,0,0,0,0,0,0),4)[1:44]
s7 <- rep(c(0,0,0,0,1,0,0,0,0,0,0,0),4)[1:44]
s8 <- rep(c(0,0,0,0,0,1,0,0,0,0,0,0),4)[1:44]
s9 <- rep(c(0,0,0,0,0,0,1,0,0,0,0,0),4)[1:44]
s10 <- rep(c(0,0,0,0,0,0,0,1,0,0,0,0),4)[1:44]
s11 <- rep(c(0,0,0,0,0,0,0,0,1,0,0,0),4)[1:44]
s12 <- rep(c(0,0,0,0,0,0,0,0,0,1,0,0),4)[1:44]

#Final Set Up for Model
n <- ncol(Y)
d <- nrow(Y)

# Set Up Data for Poisson GLM
counts <- c(t(Y))
counts_lag1 <- c(t(Y_lag1))
counts_lag2 <- c(t(Y_lag2))

# Seasonlity variables (month)
s_1  <- rep(s1,795)
s_2  <- rep(s2,795)
s_3  <- rep(s3,795)
s_4  <- rep(s4,795)
s_5  <- rep(s5,795)
s_6  <- rep(s6,795)
s_7  <- rep(s7,795)
s_8  <- rep(s8,795)
s_9  <- rep(s9,795)
s_10  <- rep(s10,795)
s_11  <- rep(s11,795)
s_12  <- rep(s12,795)

# Demographic variables
x_1 <- rep(x1, each=44)
x_2 <- rep(x2, each=44)
x_3 <- rep(x3, each=44)
x_4 <- rep(x4, each=44)
x_5 <- rep(x5, each=44)
x_6 <- rep(x6, each=44)
x_7 <- rep(x7, each=44)

Data <- as.data.frame(cbind(counts, counts_lag1, counts_lag2, s_2, s_3, s_4, s_5, s_6, s_7, s_8, s_9, s_10, s_11, s_12, s_1, x_1, x_2, x_3, x_4, x_5, x_6, x_7))

Model <- glm(counts ~ counts_lag1 + counts_lag2 + s_2 + s_3 + s_4 + s_5 + s_6 + s_7 + s_8 + s_9 + s_10 + s_11 + s_12 + x_1 + x_2 + x_3 + x_4 + x_5 + x_6 + x_7, family=poisson, data=Data)

summary(Model)
