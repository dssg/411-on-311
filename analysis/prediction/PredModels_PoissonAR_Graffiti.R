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
ACS_2007_11 <- read.csv(file="data/ACS_2007_11_Tract_KeyVars.csv", head=TRUE)

#Open Calls Data
Calls_Tract <- read.csv(file="data/tracts_2009_countsbyday.csv", head=FALSE) 

#Open Tract Neighbors Indicator File (for CAR covariance matrix)
Tract_Neighbors <- read.csv(file="data/Tract Neighbors.csv", head=TRUE)

#List of Unique Tracts
TractList <- as.data.frame(unique(Tract_Neighbors$src_GEOID1))

#Keep Only Graffiti
Graffiti <- Calls_Tract[Calls_Tract$V3 == "GRAF",]

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

#Now Merge "PreMerge1" and TractList to find tracts in common
TractList$GEO.id2 <- TractList[,1]
PreMerge2 <- merge(PreMerge1, TractList, by=c("GEO.id2"))

# Set Up for Conditional Autoregressive Spatial Model
  # First, reduce Tract Neighbors Data to just the tracts in the data
TractList <- as.data.frame(PreMerge2[,c("GEO.id2")])
TractList$src_GEOID1 <- TractList[,1]
Tract_Neighbors1 <- merge(Tract_Neighbors, TractList, by=c("src_GEOID1"))
TractList$nbr_GEOID1 <- TractList$src_GEOID1
TractList <- TractList[,c(1,3)]
Tract_Neighbors2 <- merge(Tract_Neighbors1, TractList, by=c("nbr_GEOID1"))
# Tract Neighbors Data Still Off by One from PreMerge2 Dataset; Need to Remove to Extra Tract from Merge
TractList <- as.data.frame(unique(Tract_Neighbors2$src_GEOID1))
TractList$num <- seq(from=1, to=nrow(TractList))
TractList$GEO.id2 <- TractList[,1] 
TractList$src_GEOID1 <- TractList[,1] 
Merge <- merge(TractList, PreMerge2, by=c("GEO.id2"))
  # Sort Tract_Neighbors2 and Merge in "num"
Tract_Neighbors2 <- Tract_Neighbors2[order(Tract_Neighbors2$src_GEOID1, Tract_Neighbors2$nbr_GEOID1), ]
Tract_Neighbors3 <- merge(TractList, Tract_Neighbors2, by=c("src_GEOID1"))
Tract_Neighbors3$num_src <- Tract_Neighbors3$num
TractList$nbr_GEOID1 <- TractList[,1] 
Tract_Neighbors3 <- Tract_Neighbors3[,c(5,8,11)]
TractList <- TractList[,c(2,5)]
Tract_Neighbors4 <- merge(TractList, Tract_Neighbors3, by=c("nbr_GEOID1"))
Tract_Neighbors4$num_nbr <- Tract_Neighbors4$num
Tract_Neighbors4 <- Tract_Neighbors4[,c(3:5)]
Tract_Neighbors4 <- Tract_Neighbors4[order(Tract_Neighbors4$num_src, Tract_Neighbors4$num_nbr), ]
  #Create Objects for Input into Conditional Autoregressive Model: adj, weights, num
neighbor_list <- as.matrix(Tract_Neighbors4[Tract_Neighbors4$NODE_COUNT==1,][,c(2,3)])
adj <- as.vector(neighbor_list[,2])
weights <- as.vector(rep(1,length(adj)))
num <- as.vector(rep(0,dim(TractList)[1]))
for(i in 1:length(num)) {
  num[i] <- sum(neighbor_list[,1]==i)
} 


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
s1 <- rep(c(0,0,0,0,0,0,0,0,0,0,1,0),4)[1:44]
s2 <- rep(c(0,0,0,0,0,0,0,0,0,0,0,1),4)[1:44]

#Final Set Up for Model
n <- ncol(Y)
d <- nrow(Y)

#Model in Bugs Code.  Based on p. 184 of Gamerman and Lopes
model.str <- 'model
{
  for (t in 1:n) {
      for(i in 1:d) {
        b[i,t] ~ dnorm(0,isigma2[t])
      }
    for (i in 1:d) {
      log(mu[i,t]) <- alpha + phi1*Y_lag1[i,t] + phi2*Y_lag2[i,t] + beta1*x1[i] + beta2*x2[i] + beta3*x3[i] + beta4*x4[i] + beta5*x5[i] + beta6*x6[i] + beta7*x7[i] + gamma_2*s2[t] + gamma_3*s3[t] + gamma_3*s4[t] + gamma_5*s5[t] + gamma_6*s6[t] + gamma_7*s7[t] + gamma_8*s8[t] + gamma_9*s9[t] + gamma_10*s10[t] + gamma_11*s11[t] + gamma_12*s12[t] + b[i,t]
      Y[i,t] ~ dpois(mu[i,t])
    }
  }
  isigma2[1]     <- 1/exp(logsigma2[1])
  alpha           ~ dnorm(alpha0,itau2.alpha)
  phi1            ~ dnorm(0,itau2.phi1)
  phi2            ~ dnorm(0,itau2.phi2)
  beta1           ~ dnorm(0,itau2.beta1)
  beta2           ~ dnorm(0,itau2.beta2)
  beta3           ~ dnorm(0,itau2.beta3)
  beta4           ~ dnorm(0,itau2.beta4)
  beta5           ~ dnorm(0,itau2.beta5)
  beta6           ~ dnorm(0,itau2.beta6)
  beta7           ~ dnorm(0,itau2.beta7)
  gamma_2       ~ dnorm(0,itau2.gamma_2)
  gamma_3       ~ dnorm(0,itau2.gamma_3)
  gamma_4       ~ dnorm(0,itau2.gamma_4)
  gamma_5       ~ dnorm(0,itau2.gamma_5)
  gamma_6       ~ dnorm(0,itau2.gamma_6)
  gamma_7       ~ dnorm(0,itau2.gamma_7)
  gamma_8       ~ dnorm(0,itau2.gamma_8)
  gamma_9       ~ dnorm(0,itau2.gamma_9)
  gamma_10       ~ dnorm(0,itau2.gamma_10)
  gamma_11       ~ dnorm(0,itau2.gamma_11)
  gamma_12       ~ dnorm(0,itau2.gamma_12)
  logsigma2[1]    ~ dnorm(0,itau2.sigma)
  for (t in 2:n) {
    isigma2[t] <- 1/exp(logsigma2[t])
    logsigma2[t] ~ dnorm(logsigma2[t-1],itau2.sigma)
  }
  alpha0          ~ dunif(-10000,10000)
  itau2.alpha     ~ dgamma(1,1)
  itau2.phi1      ~ dgamma(1,1)
  itau2.phi2      ~ dgamma(1,1)
  itau2.beta1     ~ dgamma(1,1)
  itau2.beta2     ~ dgamma(1,1)
  itau2.beta3     ~ dgamma(1,1)
  itau2.beta4     ~ dgamma(1,1)
  itau2.beta5     ~ dgamma(1,1)
  itau2.beta6     ~ dgamma(1,1)
  itau2.beta7     ~ dgamma(1,1)
  itau2.gamma_2 ~ dgamma(1,1)
  itau2.gamma_3 ~ dgamma(1,1)
  itau2.gamma_4 ~ dgamma(1,1)
  itau2.gamma_5 ~ dgamma(1,1)
  itau2.gamma_6 ~ dgamma(1,1)
  itau2.gamma_7 ~ dgamma(1,1)
  itau2.gamma_8 ~ dgamma(1,1)
  itau2.gamma_9 ~ dgamma(1,1)
  itau2.gamma_10 ~ dgamma(1,1)
  itau2.gamma_11 ~ dgamma(1,1)
  itau2.gamma_12 ~ dgamma(1,1)
  itau2.sigma     ~ dgamma(1,1)
}'


model.file = file("model.bug")
writeLines(model.str, model.file)
close(model.file)

inits <- list(list(alpha0=runif(1, 0, 1), alpha=rnorm(1, 0, 1), itau2.alpha=rgamma(1, 0.5, 10), 
                   phi1=rnorm(1, 0, 1), phi2=rnorm(1, 0, 1), beta1=rnorm(1, 0, 1), beta2=rnorm(1, 0, 1), beta3=rnorm(1, 0, 1), beta4=rnorm(1, 0, 1), beta5=rnorm(1, 0, 1), beta6=rnorm(1, 0, 1), beta7=rnorm(1, 0, 1), 
                   gamma_2=rnorm(1, 0, 1), gamma_3=rnorm(1, 0, 1), gamma_4=rnorm(1, 0, 1), gamma_5=rnorm(1, 0, 1), gamma_6=rnorm(1, 0, 1), gamma_7=rnorm(1, 0, 1), gamma_8=rnorm(1, 0, 1), gamma_9=rnorm(1, 0, 1), gamma_10=rnorm(1, 0, 1), gamma_11=rnorm(1, 0, 1), gamma_12=rnorm(1, 0, 1), 
                   itau2.phi1=rgamma(1, 0.5, 10), itau2.phi2=rgamma(1, 0.5, 10), itau2.beta1=rgamma(1, 0.5, 10), itau2.beta2=rgamma(1, 0.5, 10), itau2.beta3=rgamma(1, 0.5, 10), itau2.beta4=rgamma(1, 0.5, 10), itau2.beta5=rgamma(1, 0.5, 10), itau2.beta6=rgamma(1, 0.5, 10), itau2.beta7=rgamma(1, 0.5, 10), 
                   itau2.gamma_2=rgamma(1, 0.5, 10), itau2.gamma_3=rgamma(1, 0.5, 10), itau2.gamma_4=rgamma(1, 0.5, 10), itau2.gamma_5=rgamma(1, 0.5, 10), itau2.gamma_6=rgamma(1, 0.5, 10), itau2.gamma_7=rgamma(1, 0.5, 10), itau2.gamma_8=rgamma(1, 0.5, 10), itau2.gamma_9=rgamma(1, 0.5, 10), itau2.gamma_10=rgamma(1, 0.5, 10), itau2.gamma_11=rgamma(1, 0.5, 10), itau2.gamma_12=rgamma(1, 0.5, 10), 
                   logsigma2=rnorm(n, 0, 1), itau2.sigma = rgamma(1, 0.5, 10)    
                   ))
parameters <- c("alpha0", "alpha", "itau2.alpha", "phi1", "phi2", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6", "beta7", "gamma_2", "gamma_3", "gamma_4", "gamma_5", "gamma_6", "gamma_7", "gamma_8", "gamma_9", "gamma_10", "gamma_11", "gamma_12", "itau2.phi1", "itau2.phi2", "itau2.beta1", "itau2.beta2", "itau2.beta3", "itau2.beta4", "itau2.beta5", "itau2.beta6", "itau2.beta7", "itau2.gamma_2", "itau2.gamma_3", "itau2.gamma_4", "itau2.gamma_5", "itau2.gamma_6", "itau2.gamma_7", "itau2.gamma_8", "itau2.gamma_9", "itau2.gamma_10", "itau2.gamma_11", "itau2.gamma_12", "isigma2", "logsigma2", "itau2.sigma")

data <- list(Y=Y, Y_lag1=Y_lag1, Y_lag2=Y_lag2, x1=x1, x2=x2, x3=x3, x4=x4, x5=x5, x6=x6, x7=x7, s2=s2, s3=s3, s4=s4, s5=s5, s6=s6, s7=s7, s8=s8, s9=s9, s10=s10, s11=s11, s12=s12, d=d, n=n) 
#load.sim <- jags(data, inits, parameters, "model.bug", n.chains=1, n.iter=2000, n.burnin=200, progress.bar="text")

#traceplot(load.sim)

#sim.mcmc <- as.mcmc(load.sim)
#summary(sim.mcmc)


# Set Up Data for Poisson GLM

counts <- c(t(Y))
counts_lag1 <- c(t(Y_lag1))
counts_lag2 <- c(t(Y_lag2))

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


x_1 <- rep(x1, each=44)
x_2 <- rep(x2, each=44)
x_3 <- rep(x3, each=44)
x_4 <- rep(x4, each=44)
x_5 <- rep(x5, each=44)
x_6 <- rep(x6, each=44)
x_7 <- rep(x7, each=44)

#Group on time for clustering
group = rep(1:44, 795)

Data <- as.data.frame(cbind(counts, counts_lag1, counts_lag2, s_2, s_3, s_4, s_5, s_6, s_7, s_8, s_9, s_10, s_11, s_12, s_1, x_1, x_2, x_3, x_4, x_5, x_6, x_7))

#Poisson AR(2) Model
Model <- glmmML(counts ~ counts_lag1 + counts_lag2 + s_2 + s_3 + s_4 + s_5 + s_6 + s_7 + s_8 + s_9 + s_10 + s_11 + s_12 + x_1 + x_2 + x_3 + x_4 + x_5 + x_6 + x_7, family=poisson, data=Data, cluster=group)
Model2 <- glm(counts ~ counts_lag1 + counts_lag2 + s_2 + s_3 + s_4 + s_5 + s_6 + s_7 + s_8 + s_9 + s_10 + s_11 + s_12 + x_1 + x_2 + x_3 + x_4 + x_5 + x_6 + x_7, family=poisson, data=Data)

#Model Summary
summary(Model)
summary(Model2)
#PseudoR2(Model2)








