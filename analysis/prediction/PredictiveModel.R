

setwd("/mnt/data1/Indices/PredictiveModel")

#Load Necessary Packages
library(rbugs)
library(coda)
library(doBy)
library(reshape)

#Open ACS Data
ACS_2007_11 <- read.csv(file="ACS_2007_11_Tract_KeyVars.csv", head=TRUE)

#Open Calls Data
Calls_Tract <- read.csv(file="tracts_2009_countsbyday.csv", head=FALSE) 

#Keep Only Potholes
Potholes <- Calls_Tract[Calls_Tract$V3 == "PHF",]

#Remove Huge Dataset from Workspace
rm(Calls_Tract)

#Create Month and Year Variables
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

#Code additional ACS Variables
Merge$PctElderly <- 100*Merge$Age_65_older/Merge$Total_Pop
Merge$PctBlack <- 100*Merge$Black_NonHisp/Merge$Total_Pop
Merge$PctHisp <- 100*Merge$Hisp/Merge$Total_Pop

#Choose X's for Model
x1 <- Merge$Total_Pop
x2 <- Merge$PctElderly
x3 <- Merge$PctBlack
x4 <- Merge$PctHisp
x5 <- as.numeric(Merge$Pct_Fam_Below_PovLine)
x6 <- as.numeric(Merge$Unemp_Rate)


#Final Set Up for BUGS Model
n <- ncol(Y)
d <- nrow(Y)
Y <- as.matrix(Y)

#Model in Bugs Code.  Based on p. 184 of Gamerman and Lopes
model.str <- 'model
{
  for (t in 1:n) {
    for(i in 1:d) {
      b[i,t] ~ dnorm(0,isigma2[t])
    }
    for (i in 1:d) {
      log(mu[i, t]) <- alpha[t] + beta1*x1[i] +  beta2*x2[i] + beta3*x3[i] + b[i,t]
      Y[i, t] ~ dpois(mu[i, t])
    }
  }
  isigma2[1] <- 1/exp(logsigma2[1])
  alpha[1] ~ dnorm(alpha0,itau2.alpha)
  beta1 ~ dnorm(0,itau2.beta1)
  beta2 ~ dnorm(0,itau2.beta2)
  beta3 ~ dnorm(0,itau2.beta3)
  logsigma2[1] ~ dnorm(0,itau2.sigma)
  for (t in 2:n) {
    isigma2[t] <- 1/exp(logsigma2[t])
    logsigma2[t] ~ dnorm(logsigma2[t-1],itau2.sigma)
    alpha[t] ~ dnorm(alpha[t-1],itau2.alpha)
  }
  alpha0 ~ dflat()
  itau2.beta1 ~ dgamma(1,1)
  itau2.beta2 ~ dgamma(1,1)
  itau2.beta3 ~ dgamma(1,1)
  itau2.alpha ~ dgamma(1,1)
  itau2.sigma ~ dgamma(1,1)
}'


model.file = file("model.bug")
writeLines(model.str, model.file)
close(model.file)

inits <- list(list(alpha0=rnorm(1, 0, 1), alpha=rnorm(n, 0, 1), itau2.alpha=rgamma(1, 0.1, 1),
                   beta1=rnorm(1, 0, 1), beta2=rnorm(1, 0, 1), beta3=rnorm(1, 0, 1),
                   itau2.beta1=rgamma(1, 0.5, 10), itau2.beta2=rgamma(1, 0.5, 10), itau2.beta3=rgamma(1, 0.5, 10), 
                   logsigma2=rnorm(n, 0, 1), itau2.sigma = rgamma(1, 0.5, 10)    
                   ))
parameters <- c("alpha0", "alpha", "itau2.alpha", "beta1", "beta2", "beta3", "itau2.beta1", "itau2.beta2", "itau2.beta3", "isigma2", "logsigma2", "itau2.sigma")

data <- list(Y=Y, x1=x1, x2=x2, x3=x3, d=d, n=n) 
load.sim <- rbugs(data, inits, parameters, "model.bug", 
                    verbose=T, 
                    n.chains=1, n.iter=50, 
                    bugsWorkingDir="/mnt/data1/Indices/PredictiveModel", bugs="/usr/local/bin/OpenBUGS", 
                    cleanBugsWorkingDir = T)

load.mcmc <- rbugs2coda(load.sim)
summary(load.mcmc)
effectiveSize(load.mcmc)

save.image()



