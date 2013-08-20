rm(list = ls())

setwd("C:/Users/Zach/Documents/UrbanCCD/Streetlights/FinalCode")

#Load Necessary Packages
library(plyr)
library(glmmML)


#Open Alley Lights and Crime
Alley.Lights.Pois <- read.csv(file="Alley_Lights_and_Crime_for_Comm_Area_Est.csv", head=TRUE)


#Create Output Tables
Table.Alley  <- data.frame(matrix(ncol = 3, nrow = 77))
Table.Alley  <- rename(Table.Alley,  c("X7"="PctDiff.Robbery", "X8"="PVal.Robbery", "X9"="Star.Robbery"))


                       
Table.Alley[is.na(Table.Alley)] <- ""   



# Loop to run Regressions and Put Outputs in Table 


for (i in 1:77) {
  x <- sum(Alley.Lights.Pois$Robbery[Alley.Lights.Pois$community_area==i])
  if (x > 20) { 
    Fit.Alley.Robbery.Seas    <- try(glmmboot(Robbery         ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, cluster=Service.Request.No, family=poisson(link=log), data=subset(Alley.Lights.Pois, community_area==i)))
    if(inherits(Fit.Alley.Robbery.Seas, "try-error")) { next }
    Table.Alley[i,1] <- 100*exp(Fit.Alley.Robbery.Seas$coef[1])-100
    Table.Alley[i,2] <- 2*pt(-abs(Fit.Alley.Robbery.Seas$coef[1]/Fit.Alley.Robbery.Seas$sd[1]),df=length(Fit.Alley.Robbery.Seas$pred-1))
  }
}
                      
 
Table.Alley$Star.Robbery[which(Table.Alley$PVal.Robbery<0.01 & Table.Alley$PVal.Robbery!="")]                                          <- rep("**", length(which(Table.Alley$PVal.Robbery<0.01 & Table.Alley$PVal.Robbery!="")))
Table.Alley$Star.Robbery[which(Table.Alley$PVal.Robbery<0.05 & Table.Alley$PVal.Robbery>=0.01 & Table.Alley$PVal.Robbery!="")]         <- rep("*" , length(which(Table.Alley$PVal.Robbery<0.05 & Table.Alley$PVal.Robbery>=0.01 & Table.Alley$PVal.Robbery!="")))


Table.Alley[,1] <- round(as.numeric(Table.Alley[,1]),3)
Table.Alley[,2] <- round(as.numeric(Table.Alley[,2]),3)



#Open Alley Lights and Crime
Alley.Lights<- read.csv(file="alessandro_lights_and_crimes_alley.csv", head=TRUE)


#Change Community Areas to Numeric
Alley.Lights$community_area <- as.character(Alley.Lights$community_area)
Alley.Lights$community_area[which(nchar(Alley.Lights$community_area)==4)] <- as.numeric(substr(Alley.Lights$community_area[which(nchar(Alley.Lights$community_area)==4)],3,3))
Alley.Lights$community_area[which(nchar(Alley.Lights$community_area)==5)] <- as.numeric(substr(Alley.Lights$community_area[which(nchar(Alley.Lights$community_area)==5)],3,4))

# Remove Duplicates
Alley.Lights         <-         Alley.Lights[!duplicated(Alley.Lights$Service.Request.No),]

# Remove Community Area 0
Alley.Lights         <-         Alley.Lights[Alley.Lights$community_area        !=0,]

# Keep 5 Community Areas with Significant Results
Alley.Lights.Robbery <- Alley.Lights[which(Alley.Lights$community_area==7 | Alley.Lights$community_area==44 | Alley.Lights$community_area==53 | Alley.Lights$community_area==68 | Alley.Lights$community_area==71),]
Alley.Lights.Robbery$RateNotDuring <- 30*(Alley.Lights.Robbery$Robbery.Before + Alley.Lights.Robbery$Robbery.After)/(30 + Alley.Lights.Robbery$After.Period.Duration)
Alley.Lights.Robbery$RateDuring <- 30*Alley.Lights.Robbery$Robbery.During/Alley.Lights.Robbery$OutageDuration
Alley.Lights.Robbery$RateDiff <- Alley.Lights.Robbery$RateDuring - Alley.Lights.Robbery$RateNotDuring
Alley.Lights.Robbery <- Alley.Lights.Robbery[order(-Alley.Lights.Robbery$RateDiff),]
Alley.Lights.Robbery <- Alley.Lights.Robbery[,c("Service.Request.No","DateCreated", "DateCompleted", "Location", "Outcome", "x_coord", "y_coord", "zip_code", "ward",
                             "police_district", "community_area", "Robbery.Before", "Robbery.During", "Robbery.After", "RateNotDuring", "RateDuring", 
                             "RateDiff", "After.Period.Duration", "OutageDuration")]
Alley.Lights.Robbery <- Alley.Lights.Robbery[Alley.Lights.Robbery$RateDiff>=1.5,]
Alley.Lights.Robbery <- Alley.Lights.Robbery[order(Alley.Lights.Robbery$community_area),]








