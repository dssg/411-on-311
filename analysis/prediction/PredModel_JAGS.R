rm(list = ls())
setwd("C:/Users/Zach/Documents/UrbanCCD/PredictiveModel/")


#Load Necessary Packages
library(R2jags)
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
x1 <- Merge$Total_Pop/100 # Population in Hundreds
x2 <- Merge$PctElderly
x3 <- Merge$PctBlack
x4 <- Merge$PctHisp
x5 <- as.numeric(Merge$Pct_Fam_Below_PovLine)
x6 <- as.numeric(Merge$Unemp_Rate)

#Monthly Indicator Variables to Capture Seasonality
s1  <- rep(c(1,0,0,0,0,0,0,0,0,0,0,0),4)[1:46]
s2  <- rep(c(0,1,0,0,0,0,0,0,0,0,0,0),4)[1:46]
s3  <- rep(c(0,0,1,0,0,0,0,0,0,0,0,0),4)[1:46]
s4  <- rep(c(0,0,0,1,0,0,0,0,0,0,0,0),4)[1:46]
s5  <- rep(c(0,0,0,0,1,0,0,0,0,0,0,0),4)[1:46]
s6  <- rep(c(0,0,0,0,0,1,0,0,0,0,0,0),4)[1:46]
s7  <- rep(c(0,0,0,0,0,0,1,0,0,0,0,0),4)[1:46]
s8  <- rep(c(0,0,0,0,0,0,0,1,0,0,0,0),4)[1:46]
s9  <- rep(c(0,0,0,0,0,0,0,0,1,0,0,0),4)[1:46]
s10 <- rep(c(0,0,0,0,0,0,0,0,0,1,0,0),4)[1:46]
s11 <- rep(c(0,0,0,0,0,0,0,0,0,0,1,0),4)[1:46]
s12 <- rep(c(0,0,0,0,0,0,0,0,0,0,0,1),4)[1:46]


#Final Set Up for BUGS Model
n <- ncol(Y)
d <- nrow(Y)
Y <- as.matrix(Y)

#Model in Bugs Code.  Based on p. 184 of Gamerman and Lopes
model.str <- 'model
{
  for (t in 1:n) {
    for(i in 1:d) {
      b[i,t] ~ dnorm(0,isigma2)
    }
    for (i in 1:d) {
      log(mu[i, t]) <- alpha[t] + beta1*x1[i] + beta2*x2[i] + beta3*x3[i] + gamma2*s2[t] + gamma3*s3[t] + gamma4*s4[t] + gamma5*s5[t] + gamma6*s6[t] + gamma7*s7[t] + gamma8*s8[t] + gamma9*s9[t] + gamma10*s10[t] + gamma11*s11[t] + gamma12*s12[t] + b[i,t]
      Y[i, t] ~ dpois(mu[i, t])
    }
  }
  isigma2 <- 1/exp(logsigma2)
  alpha[1]  ~ dnorm(alpha0,itau2.alpha)
  beta1     ~ dnorm(0,itau2.beta1)
  beta2     ~ dnorm(0,itau2.beta2)
  beta3     ~ dnorm(0,itau2.beta3)
  gamma2    ~ dnorm(0,itau2.gamma2)
  gamma3    ~ dnorm(0,itau2.gamma3)
  gamma4    ~ dnorm(0,itau2.gamma4)
  gamma5    ~ dnorm(0,itau2.gamma5)
  gamma6    ~ dnorm(0,itau2.gamma6)
  gamma7    ~ dnorm(0,itau2.gamma7)
  gamma8    ~ dnorm(0,itau2.gamma8)
  gamma9    ~ dnorm(0,itau2.gamma9)
  gamma10   ~ dnorm(0,itau2.gamma10)
  gamma11   ~ dnorm(0,itau2.gamma11)
  gamma12   ~ dnorm(0,itau2.gamma12)
  logsigma2 ~ dnorm(0,itau2.sigma)
  for (t in 2:n) {
    alpha[t] ~ dnorm(alpha[t-1],itau2.alpha)
  }
  alpha0 ~ dunif(-10000,10000)
  itau2.alpha   ~ dgamma(1,1)
  itau2.beta1   ~ dgamma(1,1)
  itau2.beta2   ~ dgamma(1,1)
  itau2.beta3   ~ dgamma(1,1)
  itau2.gamma2  ~ dgamma(1,1)
  itau2.gamma3  ~ dgamma(1,1)
  itau2.gamma4  ~ dgamma(1,1)
  itau2.gamma5  ~ dgamma(1,1)
  itau2.gamma6  ~ dgamma(1,1)
  itau2.gamma7  ~ dgamma(1,1)
  itau2.gamma8  ~ dgamma(1,1)
  itau2.gamma9  ~ dgamma(1,1)
  itau2.gamma10 ~ dgamma(1,1)
  itau2.gamma11 ~ dgamma(1,1)
  itau2.gamma12 ~ dgamma(1,1)
  itau2.sigma   ~ dgamma(1,1)
}'


model.file = file("model.bug")
writeLines(model.str, model.file)
close(model.file)

inits <- list(list(alpha0=rnorm(1, 0, 1), alpha=rnorm(n, 0, 1), itau2.alpha=rgamma(1, 0.5, 10), 
                   beta1=rnorm(1, 0, 1), beta2=rnorm(1, 0, 1), beta3=rnorm(1, 0, 1), 
                   gamma2=rnorm(1, 0, 1), gamma3=rnorm(1, 0, 1), gamma4=rnorm(1, 0, 1), gamma5=rnorm(1, 0, 1), gamma6=rnorm(1, 0, 1), gamma7=rnorm(1, 0, 1), gamma8=rnorm(1, 0, 1), gamma9=rnorm(1, 0, 1), gamma10=rnorm(1, 0, 1), gamma11=rnorm(1, 0, 1), gamma12=rnorm(1, 0, 1), 
                   itau2.beta1=rgamma(1, 0.5, 10), itau2.beta2=rgamma(1, 0.5, 10), itau2.beta3=rgamma(1, 0.5, 10), 
                   itau2.gamma2=rgamma(1, 0.5, 10), itau2.gamma3=rgamma(1, 0.5, 10), itau2.gamma4=rgamma(1, 0.5, 10), itau2.gamma5=rgamma(1, 0.5, 10), itau2.gamma6=rgamma(1, 0.5, 10), itau2.gamma7=rgamma(1, 0.5, 10), itau2.gamma8=rgamma(1, 0.5, 10), itau2.gamma9=rgamma(1, 0.5, 10), itau2.gamma10=rgamma(1, 0.5, 10), itau2.gamma11=rgamma(1, 0.5, 10), itau2.gamma12=rgamma(1, 0.5, 10), 
                   logsigma2=rnorm(1, 0, 1), itau2.sigma = rgamma(1, 0.5, 10)    
                   ))
parameters <- c("alpha0", "alpha", "itau2.alpha", "beta1", "beta2", "beta3", "gamma2", "gamma3", "gamma4", "gamma5", "gamma6", "gamma7", "gamma8", "gamma9", "gamma10", "gamma11", "gamma12", "itau2.beta1", "itau2.beta2", "itau2.beta3", "itau2.gamma2", "itau2.gamma3", "itau2.gamma4", "itau2.gamma5", "itau2.gamma6", "itau2.gamma7", "itau2.gamma8", "itau2.gamma9", "itau2.gamma10", "itau2.gamma11", "itau2.gamma12", "isigma2", "logsigma2", "itau2.sigma")

data <- list(Y=Y, x1=x1, x2=x2, x3=x3, s2=s2, s3=s3, s4=s4, s5=s5, s6=s6, s7=s7, s8=s8, s9=s9, s10=s10, s11=s11, s12=s12, d=d, n=n) 
load.sim <- jags(data, inits, parameters, "model.bug", n.chains=1, n.iter=50, progress.bar="text")

summary(load.sim)
traceplot(load.sim)

sim.mcmc <- as.mcmc(load.sim)
xyplot(sim.mcmc)
densityplot(sim.mcmc)

save.image()



