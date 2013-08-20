rm(list = ls())

setwd("C:/Users/Zach/Documents/UrbanCCD/Streetlights")

#Load Necessary Packages
library(plyr)


#Open Alley Lights and Crime
Alley.Lights.Pois <- read.csv(file="Alley_Lights_and_Crime_for_Comm_Area_Est.csv", head=TRUE)

#Open One Out and Crime
Street.Lights.OneOut.Pois <- read.csv(file="Street_Lights_One_Out_and_Crime_for_Comm_Area_Est.csv", head=TRUE)

#Open All Out and Crime
Street.Lights.AllOut.Pois <- read.csv(file="Street_Lights_All_Out_and_Crime_for_Comm_Area_Est.csv", head=TRUE)


#Create Output Tables
Table.Alley  <- data.frame(matrix(ncol = 9, nrow = 77))
Table.Alley  <- rename(Table.Alley,  c("X1"="PctDiff.AllCrimes", "X2"="PVal.AllCrimes", "X3"="Star.AllCrimes", 
                                               "X4"="PctDiff.Theft",    "X5"="PVal.Theft",    "X6"="Star.Theft", 
                                               "X7"="PctDiff.Robbery", "X8"="PVal.Robbery", "X9"="Star.Robbery"))

Table.OneOut  <- data.frame(matrix(ncol = 18, nrow = 77))
Table.OneOut  <- rename(Table.OneOut,  c("X1"="PctDiff.AllCrimes",          "X2"="PVal.AllCrimes",          "X3"="Star.AllCrimes", 
                                                "X4"="PctDiff.Theft",             "X5"="PVal.Theft",             "X6"="Star.Theft", 
                                                "X7"="PctDiff.Narcotics",          "X8"="PVal.Narcotics",          "X9"="Star.Narcotics", 
                                                "X10"="PctDiff.Battery",           "X11"="PVal.Battery",           "X12"="Star.Battery", 
                                                "X13"="PctDiff.MotorVehicleTheft", "X14"="PVal.MotorVehicleTheft", "X15"="Star.MotorVehicleTheft",
                                                "X16"="PctDiff.Robbery",           "X17"="PVal.Robbery",           "X18"="Star.Robbery"))

Table.AllOut  <- data.frame(matrix(ncol = 12, nrow = 77))
Table.AllOut  <- rename(Table.AllOut,  c("X1"="PctDiff.AllCrimes",          "X2"="PVal.AllCrimes",          "X3"="Star.AllCrimes", 
                                                "X4"="PctDiff.Narcotics",          "X5"="PVal.Narcotics",          "X6"="Star.Narcotics", 
                                                "X7"="PctDiff.CriminalDamage",     "X8"="PVal.CriminalDamage",     "X9"="Star.CriminalDamage", 
                                                "X10"="PctDiff.Assault",           "X11"="PVal.Assault",           "X12"="Star.Assault"))
                       
Table.Alley[is.na(Table.Alley)] <- ""   
Table.OneOut[is.na(Table.OneOut)] <- "" 
Table.AllOut[is.na(Table.AllOut)] <- "" 


#Loops to run Regressions and Put Outputs in Table 

for (i in 1:77) {
  x <- sum(Alley.Lights.Pois$AllCrimes[Alley.Lights.Pois$community_area==i])
  if (x > 20) {
    Fit.Alley.AllCrimes.Seas    <- try(glm(AllCrimes         ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=subset(Alley.Lights.Pois, community_area==i))) 
    if(inherits(Fit.Alley.AllCrimes.Seas, "try-error")) { next }
    Table.Alley[i,1] <- 100*exp(Fit.Alley.AllCrimes.Seas$coef[2])-100
    Table.Alley[i,2] <- summary(Fit.Alley.AllCrimes.Seas)$coef[2,4]
  }
}

for (i in 1:77) {
  x <- sum(Alley.Lights.Pois$Theft[Alley.Lights.Pois$community_area==i])
  if (x > 20) {
    Fit.Alley.Theft.Seas       <- try(glm(Theft            ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=subset(Alley.Lights.Pois, community_area==i)))
    if(inherits(Fit.Alley.Theft.Seas, "try-error")) { next }
    Table.Alley[i,4] <- 100*exp(Fit.Alley.Theft.Seas$coef[2])-100
    Table.Alley[i,5] <- summary(Fit.Alley.Theft.Seas)$coef[2,4]
  }
}

for (i in 1:77) {
  x <- sum(Alley.Lights.Pois$Robbery[Alley.Lights.Pois$community_area==i])
  if (x > 20) { 
    Fit.Alley.Robbery.Seas    <- try(glm(Robbery         ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=subset(Alley.Lights.Pois, community_area==i)))
    if(inherits(Fit.Alley.Robbery.Seas, "try-error")) { next }
    Table.Alley[i,7] <- 100*exp(Fit.Alley.Robbery.Seas$coef[2])-100
    Table.Alley[i,8] <- summary(Fit.Alley.Robbery.Seas)$coef[2,4]
  }
}
                      
             
     
for (i in 1:77) {
  x <- sum(Street.Lights.OneOut.Pois$AllCrimes[Street.Lights.OneOut.Pois$community_area==i])
  if (x > 20) {
    Fit.OneOut.AllCrimes.Seas         <- try(glm(AllCrimes         ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=subset(Street.Lights.OneOut.Pois, community_area==i)))
    if(inherits(Fit.OneOut.AllCrimes.Seas, "try-error")) { next }
    Table.OneOut[i,1] <- 100*exp(Fit.OneOut.AllCrimes.Seas$coef[2])-100
    Table.OneOut[i,2] <- summary(Fit.OneOut.AllCrimes.Seas)$coef[2,4]
  }
}

for (i in 1:77) {
  x <- sum(Street.Lights.OneOut.Pois$Theft[Street.Lights.OneOut.Pois$community_area==i])
  if (x > 20) {  
    Fit.OneOut.Theft.Seas            <- try(glm(Theft            ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=subset(Street.Lights.OneOut.Pois, community_area==i)))
    if(inherits(Fit.OneOut.Theft.Seas, "try-error")) { next }
    Table.OneOut[i,4] <- 100*exp(Fit.OneOut.Theft.Seas$coef[2])-100
    Table.OneOut[i,5] <- summary(Fit.OneOut.Theft.Seas)$coef[2,4]
  }
}

for (i in 1:77) {
  x <- sum(Street.Lights.OneOut.Pois$Narcotics[Street.Lights.OneOut.Pois$community_area==i])
  if (x > 20) { 
    Fit.OneOut.Narcotics.Seas         <- try(glm(Narcotics         ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=subset(Street.Lights.OneOut.Pois, community_area==i)))
    if(inherits(Fit.OneOut.Narcotics.Seas, "try-error")) { next }
    Table.OneOut[i,7] <- 100*exp(Fit.OneOut.Narcotics.Seas$coef[2])-100
    Table.OneOut[i,8] <- summary(Fit.OneOut.Narcotics.Seas)$coef[2,4]
  }
}

for (i in 1:77) {
  x <- sum(Street.Lights.OneOut.Pois$Battery[Street.Lights.OneOut.Pois$community_area==i])
  if (x > 20) { 
    Fit.OneOut.Battery.Seas           <- try(glm(Battery         ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=subset(Street.Lights.OneOut.Pois, community_area==i)))
    if(inherits(Fit.OneOut.Battery.Seas, "try-error")) { next }
    Table.OneOut[i,10] <- 100*exp(Fit.OneOut.Battery.Seas$coef[2])-100
    Table.OneOut[i,11] <- summary(Fit.OneOut.Battery.Seas)$coef[2,4]
  }
}

for (i in 1:77) {
x <- sum(Street.Lights.OneOut.Pois$MotorVehicleTheft[Street.Lights.OneOut.Pois$community_area==i])
  if (x > 20) { 
    Fit.OneOut.MotorVehicleTheft.Seas <- try(glm(MotorVehicleTheft         ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=subset(Street.Lights.OneOut.Pois, community_area==i)))
    if(inherits(Fit.OneOut.MotorVehicleTheft.Seas, "try-error")) { next }
    Table.OneOut[i,13] <- 100*exp(Fit.OneOut.MotorVehicleTheft.Seas$coef[2])-100
    Table.OneOut[i,14] <- summary(Fit.OneOut.MotorVehicleTheft.Seas)$coef[2,4]
  }
}

for (i in 1:77) {
  x <- sum(Street.Lights.OneOut.Pois$Robbery[Street.Lights.OneOut.Pois$community_area==i])
  if (x > 20) { 
    Fit.OneOut.Robbery.Seas           <- try(glm(Robbery         ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=subset(Street.Lights.OneOut.Pois, community_area==i)))
    if(inherits(Fit.OneOut.Robbery.Seas, "try-error")) { next }
    Table.OneOut[i,16] <- 100*exp(Fit.OneOut.Robbery.Seas$coef[2])-100
    Table.OneOut[i,17] <- summary(Fit.OneOut.Robbery.Seas)$coef[2,4]
  }
}


for (i in 1:77) {
  x <- sum(Street.Lights.AllOut.Pois$AllCrimes[Street.Lights.AllOut.Pois$community_area==i])
  if (x > 20) {
    Fit.AllOut.AllCrimes.Seas      <- try(glm(AllCrimes         ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=subset(Street.Lights.AllOut.Pois, community_area==i)))
    if(inherits(Fit.AllOut.AllCrimes.Seas, "try-error")) { next }
    Table.AllOut[i,1] <- 100*exp(Fit.AllOut.AllCrimes.Seas$coef[2])-100
    Table.AllOut[i,2] <- summary(Fit.AllOut.AllCrimes.Seas)$coef[2,4]
  }
}

for (i in 1:77) {
  x <- sum(Street.Lights.AllOut.Pois$Narcotics[Street.Lights.AllOut.Pois$community_area==i])
  if (x > 20) {
    Fit.AllOut.Narcotics.Seas      <- try(glm(Narcotics         ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=subset(Street.Lights.AllOut.Pois, community_area==i)))
    if(inherits(Fit.AllOut.Narcotics.Seas, "try-error")) { next }
    Table.AllOut[i,4] <- 100*exp(Fit.AllOut.Narcotics.Seas$coef[2])-100
    Table.AllOut[i,5] <- summary(Fit.AllOut.Narcotics.Seas)$coef[2,4]
  }
}

for (i in 1:77) {
  x <- sum(Street.Lights.AllOut.Pois$CriminalDamage[Street.Lights.AllOut.Pois$community_area==i])
  if (x > 20) {
    Fit.AllOut.CriminalDamage.Seas <- try(glm(CriminalDamage         ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=subset(Street.Lights.AllOut.Pois, community_area==i)))
    if(inherits(Fit.AllOut.CriminalDamage.Seas, "try-error")) { next }
    Table.AllOut[i,7] <- 100*exp(Fit.AllOut.CriminalDamage.Seas$coef[2])-100
    Table.AllOut[i,8] <- summary(Fit.AllOut.CriminalDamage.Seas)$coef[2,4]
  }
}

for (i in 1:77) {
  x <- sum(Street.Lights.AllOut.Pois$Assault[Street.Lights.AllOut.Pois$community_area==i])
  if (x > 20) {
    Fit.AllOut.Assault.Seas        <- try(glm(Assault         ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=subset(Street.Lights.AllOut.Pois, community_area==i)))
    if(inherits(Fit.AllOut.Assault.Seas, "try-error")) { next }
    Table.AllOut[i,10] <- 100*exp(Fit.AllOut.Assault.Seas$coef[2])-100
    Table.AllOut[i,11] <- summary(Fit.AllOut.Assault.Seas)$coef[2,4]
  }
}

Table.Alley$Star.AllCrimes[which(Table.Alley$PVal.AllCrimes<0.01 & Alley$PVal.AllCrimes!="")]                                          <- rep("**", length(which(Table.Alley$PVal.AllCrimes<0.01 & Table.Alley$PVal.AllCrimes!="")))
Table.Alley$Star.AllCrimes[which(Table.Alley$PVal.AllCrimes<0.05 & Table.Alley$PVal.AllCrimes>=0.01 & Table.Alley$PVal.AllCrimes!="")] <- rep("*" , length(which(Table.Alley$PVal.AllCrimes<0.05 & Table.Alley$PVal.AllCrimes>=0.01 & Table.Alley$PVal.AllCrimes!="")))
Table.Alley$Star.Theft[which(Table.Alley$PVal.Theft<0.01 & Table.Alley$PVal.Theft!="")]                                                <- rep("**", length(which(Table.Alley$PVal.Theft<0.01 & Table.Alley$PVal.Theft!="")))
Table.Alley$Star.Theft[which(Table.Alley$PVal.Theft<0.05 & Table.Alley$PVal.Theft>=0.01 & Table.Alley$PVal.Theft!="")]                 <- rep("*" , length(which(Table.Alley$PVal.Theft<0.05 & Table.Alley$PVal.Theft>=0.01 & Table.Alley$PVal.Theft!="")))
Table.Alley$Star.Robbery[which(Table.Alley$PVal.Robbery<0.01 & Table.Alley$PVal.Robbery!="")]                                          <- rep("**", length(which(Table.Alley$PVal.Robbery<0.01 & Table.Alley$PVal.Robbery!="")))
Table.Alley$Star.Robbery[which(Table.Alley$PVal.Robbery<0.05 & Table.Alley$PVal.Robbery>=0.01 & Table.Alley$PVal.Robbery!="")]         <- rep("*" , length(which(Table.Alley$PVal.Robbery<0.05 & Table.Alley$PVal.Robbery>=0.01 & Table.Alley$PVal.Robbery!="")))

Table.OneOut$Star.AllCrimes[which(Table.OneOut$PVal.AllCrimes<0.01 & Table.OneOut$PVal.AllCrimes!="")]                                                                     <- rep("**", length(which(Table.OneOut$PVal.AllCrimes<0.01 & Table.OneOut$PVal.AllCrimes!="")))
Table.OneOut$Star.AllCrimes[which(Table.OneOut$PVal.AllCrimes<0.05 & Table.OneOut$PVal.AllCrimes>=0.01 & Table.OneOut$PVal.AllCrimes!="")]                                 <- rep("*" , length(which(Table.OneOut$PVal.AllCrimes<0.05 & Table.OneOut$PVal.AllCrimes>=0.01 & Table.OneOut$PVal.AllCrimes!="")))
Table.OneOut$Star.Theft[which(Table.OneOut$PVal.Theft<0.01 & Table.OneOut$PVal.Theft!="")]                                                                                 <- rep("**", length(which(Table.OneOut$PVal.Theft<0.01 & Table.OneOut$PVal.Theft!="")))
Table.OneOut$Star.Theft[which(Table.OneOut$PVal.Theft<0.05 & Table.OneOut$PVal.Theft>=0.01 & Table.OneOut$PVal.Theft!="")]                                                 <- rep("*" , length(which(Table.OneOut$PVal.Theft<0.05 & Table.OneOut$PVal.Theft>=0.01 & Table.OneOut$PVal.Theft!="")))
Table.OneOut$Star.Narcotics[which(Table.OneOut$PVal.Narcotics<0.01 & Table.OneOut$PVal.Narcotics!="")]                                                                     <- rep("**", length(which(Table.OneOut$PVal.Narcotics<0.01 & Table.OneOut$PVal.Narcotics!="")))
Table.OneOut$Star.Narcotics[which(Table.OneOut$PVal.Narcotics<0.05 & Table.OneOut$PVal.Narcotics>=0.01 & Table.OneOut$PVal.Narcotics!="")]                                 <- rep("*" , length(which(Table.OneOut$PVal.Narcotics<0.05 & Table.OneOut$PVal.Narcotics>=0.01 & Table.OneOut$PVal.Narcotics!="")))
Table.OneOut$Star.Battery[which(Table.OneOut$PVal.Battery<0.01 & Table.OneOut$PVal.Battery!="")]                                                                           <- rep("**", length(which(Table.OneOut$PVal.Battery<0.01 & Table.OneOut$PVal.Battery!="")))
Table.OneOut$Star.Battery[which(Table.OneOut$PVal.Battery<0.05 & Table.OneOut$PVal.Battery>=0.01 & Table.OneOut$PVal.Battery!="")]                                         <- rep("*" , length(which(Table.OneOut$PVal.Battery<0.05 & Table.OneOut$PVal.Battery>=0.01 & Table.OneOut$PVal.Battery!="")))
Table.OneOut$Star.MotorVehicleTheft[which(Table.OneOut$PVal.MotorVehicleTheft<0.01 & Table.OneOut$PVal.MotorVehicleTheft!="")]                                             <- rep("**", length(which(Table.OneOut$PVal.MotorVehicleTheft<0.01 & Table.OneOut$PVal.MotorVehicleTheft!="")))
Table.OneOut$Star.MotorVehicleTheft[which(Table.OneOut$PVal.MotorVehicleTheft<0.05 & Table.OneOut$PVal.MotorVehicleTheft>=0.01 & Table.OneOut$PVal.MotorVehicleTheft!="")] <- rep("*" , length(which(Table.OneOut$PVal.MotorVehicleTheft<0.05 & Table.OneOut$PVal.MotorVehicleTheft>=0.01 & Table.OneOut$PVal.MotorVehicleTheft!="")))
Table.OneOut$Star.Robbery[which(Table.OneOut$PVal.Robbery<0.01 & Table.OneOut$PVal.Robbery!="")]                                                                           <- rep("**", length(which(Table.OneOut$PVal.Robbery<0.01 & Table.OneOut$PVal.Robbery!="")))
Table.OneOut$Star.Robbery[which(Table.OneOut$PVal.Robbery<0.05 & Table.OneOut$PVal.Robbery>=0.01 & Table.OneOut$PVal.Robbery!="")]                                         <- rep("*" , length(which(Table.OneOut$PVal.Robbery<0.05 & Table.OneOut$PVal.Robbery>=0.01 & Table.OneOut$PVal.Robbery!="")))
Table.OneOut$Star.Burglary[which(Table.OneOut$PVal.Burglary<0.01 & Table.OneOut$PVal.Burglary!="")]                                                                        <- rep("**", length(which(Table.OneOut$PVal.Burglary<0.01 & Table.OneOut$PVal.Burglary!="")))
Table.OneOut$Star.Burglary[which(Table.OneOut$PVal.Burglary<0.05 & Table.OneOut$PVal.Burglary>=0.01 & Table.OneOut$PVal.Burglary!="")]                                     <- rep("*" , length(which(Table.OneOut$PVal.Burglary<0.05 & Table.OneOut$PVal.Burglary>=0.01 & Table.OneOut$PVal.Burglary!="")))

Table.AllOut$Star.AllCrimes[which(Table.AllOut$PVal.AllCrimes<0.01 & Table.AllOut$PVal.AllCrimes!="")]                                                         <- rep("**", length(which(Table.AllOut$PVal.AllCrimes<0.01 & Table.AllOut$PVal.AllCrimes!="")))
Table.AllOut$Star.AllCrimes[which(Table.AllOut$PVal.AllCrimes<0.05 & Table.AllOut$PVal.AllCrimes>=0.01 & Table.AllOut$PVal.AllCrimes!="")]                     <- rep("*" , length(which(Table.AllOut$PVal.AllCrimes<0.05 & Table.AllOut$PVal.AllCrimes>=0.01 & Table.AllOut$PVal.AllCrimes!="")))
Table.AllOut$Star.Narcotics[which(Table.AllOut$PVal.Narcotics<0.01 & Table.AllOut$PVal.Narcotics!="")]                                                         <- rep("**", length(which(Table.AllOut$PVal.Narcotics<0.01 & Table.AllOut$PVal.Narcotics!="")))
Table.AllOut$Star.Narcotics[which(Table.AllOut$PVal.Narcotics<0.05 & Table.AllOut$PVal.Narcotics>=0.01 & Table.AllOut$PVal.Narcotics!="")]                     <- rep("*" , length(which(Table.AllOut$PVal.Narcotics<0.05 & Table.AllOut$PVal.Narcotics>=0.01 & Table.AllOut$PVal.Narcotics!="")))
Table.AllOut$Star.CriminalDamage[which(Table.AllOut$PVal.CriminalDamage<0.01 & Table.AllOut$PVal.CriminalDamage!="")]                                          <- rep("**", length(which(Table.AllOut$PVal.CriminalDamage<0.01 & Table.AllOut$PVal.CriminalDamage!="")))
Table.AllOut$Star.CriminalDamage[which(Table.AllOut$PVal.CriminalDamage<0.05 & Table.AllOut$PVal.CriminalDamage>=0.01 & Table.AllOut$PVal.CriminalDamage!="")] <- rep("*" , length(which(Table.AllOut$PVal.CriminalDamage<0.05 & Table.AllOut$PVal.CriminalDamage>=0.01 & Table.AllOut$PVal.CriminalDamage!="")))
Table.AllOut$Star.Assault[which(Table.AllOut$PVal.Assault<0.01 & Table.AllOut$PVal.Assault!="")]                                                               <- rep("**", length(which(Table.AllOut$PVal.Assault<0.01 & Table.AllOut$PVal.Assault!="")))
Table.AllOut$Star.Assault[which(Table.AllOut$PVal.Assault<0.05 & Table.AllOut$PVal.Assault>=0.01 & Table.AllOut$PVal.Assault!="")]                             <- rep("*" , length(which(Table.AllOut$PVal.Assault<0.05 & Table.AllOut$PVal.Assault>=0.01 & Table.AllOut$PVal.Assault!="")))

Table.Alley[,1] <- round(as.numeric(Table.Alley[,1]),3)
Table.Alley[,2] <- round(as.numeric(Table.Alley[,2]),3)
Table.Alley[,4] <- round(as.numeric(Table.Alley[,4]),3)
Table.Alley[,5] <- round(as.numeric(Table.Alley[,5]),3)
Table.Alley[,7] <- round(as.numeric(Table.Alley[,7]),3)
Table.Alley[,8] <- round(as.numeric(Table.Alley[,8]),3)

Table.OneOut[,1] <- round(as.numeric(Table.OneOut[,1]),3)
Table.OneOut[,2] <- round(as.numeric(Table.OneOut[,2]),3)
Table.OneOut[,4] <- round(as.numeric(Table.OneOut[,4]),3)
Table.OneOut[,5] <- round(as.numeric(Table.OneOut[,5]),3)
Table.OneOut[,7] <- round(as.numeric(Table.OneOut[,7]),3)
Table.OneOut[,8] <- round(as.numeric(Table.OneOut[,8]),3)
Table.OneOut[,10] <- round(as.numeric(Table.OneOut[,10]),3)
Table.OneOut[,11] <- round(as.numeric(Table.OneOut[,11]),3)
Table.OneOut[,13] <- round(as.numeric(Table.OneOut[,13]),3)
Table.OneOut[,14] <- round(as.numeric(Table.OneOut[,14]),3)
Table.OneOut[,16] <- round(as.numeric(Table.OneOut[,16]),3)
Table.OneOut[,17] <- round(as.numeric(Table.OneOut[,17]),3)

Table.AllOut[,1] <- round(as.numeric(Table.AllOut[,1]),3)
Table.AllOut[,2] <- round(as.numeric(Table.AllOut[,2]),3)
Table.AllOut[,4] <- round(as.numeric(Table.AllOut[,4]),3)
Table.AllOut[,5] <- round(as.numeric(Table.AllOut[,5]),3)
Table.AllOut[,7] <- round(as.numeric(Table.AllOut[,7]),3)
Table.AllOut[,8] <- round(as.numeric(Table.AllOut[,8]),3)
Table.AllOut[,10] <- round(as.numeric(Table.AllOut[,10]),3)
Table.AllOut[,11] <- round(as.numeric(Table.AllOut[,11]),3)






