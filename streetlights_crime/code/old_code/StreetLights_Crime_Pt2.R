rm(list = ls())

#Load Necessary Packages
library(plyr)
library(doBy)
library(reshape)


setwd("C:/Users/Zach/Documents/UrbanCCD/Streetlights")

#Open Alley Lights and Crime
Alley.Lights<- read.csv(file="alessandro_lights_and_crimes_alley.csv", head=TRUE)

#Open Street Lights One Out and Crime
Street.Lights.OneOut<- read.csv(file="alessandro_lights_and_crimes_one-out.csv", head=TRUE)

#Open Street Lights All Out and Crime
Street.Lights.AllOut<- read.csv(file="alessandro_lights_and_crimes_all-out.csv", head=TRUE)

#Change Community Areas to Numeric
Alley.Lights$community_area <- as.character(Alley.Lights$community_area)
Street.Lights.OneOut$community_area <- as.character(Street.Lights.OneOut$community_area)
Street.Lights.AllOut$community_area <- as.character(Street.Lights.AllOut$community_area)

Alley.Lights$community_area[which(nchar(Alley.Lights$community_area)==4)] <- as.numeric(substr(Alley.Lights$community_area[which(nchar(Alley.Lights$community_area)==4)],3,3))
Alley.Lights$community_area[which(nchar(Alley.Lights$community_area)==5)] <- as.numeric(substr(Alley.Lights$community_area[which(nchar(Alley.Lights$community_area)==5)],3,4))
Street.Lights.OneOut$community_area[which(nchar(Street.Lights.OneOut$community_area)==4)] <- as.numeric(substr(Street.Lights.OneOut$community_area[which(nchar(Street.Lights.OneOut$community_area)==4)],3,3))
Street.Lights.OneOut$community_area[which(nchar(Street.Lights.OneOut$community_area)==5)] <- as.numeric(substr(Street.Lights.OneOut$community_area[which(nchar(Street.Lights.OneOut$community_area)==5)],3,4))
Street.Lights.AllOut$community_area[which(nchar(Street.Lights.AllOut$community_area)==4)] <- as.numeric(substr(Street.Lights.AllOut$community_area[which(nchar(Street.Lights.AllOut$community_area)==4)],3,3))
Street.Lights.AllOut$community_area[which(nchar(Street.Lights.AllOut$community_area)==5)] <- as.numeric(substr(Street.Lights.AllOut$community_area[which(nchar(Street.Lights.AllOut$community_area)==5)],3,4))

# Remove Duplicates
Alley.Lights         <-         Alley.Lights[!duplicated(Alley.Lights$Service.Request.No),]
Street.Lights.OneOut <- Street.Lights.OneOut[!duplicated(Street.Lights.OneOut$Service.Request.No),]
Street.Lights.AllOut <- Street.Lights.AllOut[!duplicated(Street.Lights.AllOut$Service.Request.No),]


# Remove Community Area 0
Alley.Lights         <-         Alley.Lights[Alley.Lights$community_area        !=0,]
Street.Lights.OneOut <- Street.Lights.OneOut[Street.Lights.OneOut$community_area!=0,]
Street.Lights.AllOut <- Street.Lights.AllOut[Street.Lights.AllOut$community_area!=0,]


#Create Summary Tables
Summary.Table.Alley  <- data.frame(matrix(ncol = 4, nrow = 11), row.names = c("All Crimes (No Deceptive Practice)", 
                                                                              "Theft", "Narcotics", "Battery", "Criminal Damage", "Motor Vehicle Theft", 
                                                                               "Robbery", "Assault", "Burglary", "Homicide", "Deceptive Practice"))
Summary.Table.OneOut <- data.frame(matrix(ncol = 4, nrow = 11), row.names = c("All Crimes (No Deceptive Practice)", 
                                                                              "Theft", "Narcotics", "Battery", "Criminal Damage", "Motor Vehicle Theft", 
                                                                              "Robbery", "Assault", "Burglary", "Homicide", "Deceptive Practice"))
Summary.Table.AllOut <- data.frame(matrix(ncol = 4, nrow = 11), row.names = c("All Crimes (No Deceptive Practice)", 
                                                                              "Theft", "Narcotics", "Battery", "Criminal Damage", "Motor Vehicle Theft", 
                                                                              "Robbery", "Assault", "Burglary", "Homicide", "Deceptive Practice"))
Summary.Table.Alley  <- rename(Summary.Table.Alley,  c("X1"="Before", "X2"="During", "X3"="After", "X4"="AvgBeforeAfter"))
Summary.Table.OneOut <- rename(Summary.Table.OneOut, c("X1"="Before", "X2"="During", "X3"="After", "X4"="AvgBeforeAfter"))
Summary.Table.AllOut <- rename(Summary.Table.AllOut, c("X1"="Before", "X2"="During", "X3"="After", "X4"="AvgBeforeAfter"))



Summary.Table.Alley[1,1]  <- 30*sum(Alley.Lights$Crimes.All.Before)       /(30*nrow(Alley.Lights))
Summary.Table.Alley[2,1]  <- 30*sum(Alley.Lights$Theft.Before)            /(30*nrow(Alley.Lights))
Summary.Table.Alley[3,1]  <- 30*sum(Alley.Lights$Narcotics.Before)        /(30*nrow(Alley.Lights))
Summary.Table.Alley[4,1]  <- 30*sum(Alley.Lights$Battery.Before)          /(30*nrow(Alley.Lights))
Summary.Table.Alley[5,1]  <- 30*sum(Alley.Lights$CriminalDamage.Before)   /(30*nrow(Alley.Lights))
Summary.Table.Alley[6,1]  <- 30*sum(Alley.Lights$MotorVehicleTheft.Before)/(30*nrow(Alley.Lights))
Summary.Table.Alley[7,1]  <- 30*sum(Alley.Lights$Robbery.Before)          /(30*nrow(Alley.Lights))
Summary.Table.Alley[8,1]  <- 30*sum(Alley.Lights$Assault.Before)          /(30*nrow(Alley.Lights))
Summary.Table.Alley[9,1]  <- 30*sum(Alley.Lights$Burglary.Before)         /(30*nrow(Alley.Lights))
Summary.Table.Alley[10,1] <- 30*sum(Alley.Lights$Homicide.Before)         /(30*nrow(Alley.Lights))
Summary.Table.Alley[11,1] <- 30*sum(Alley.Lights$DeceptivePractice.Before)/(30*nrow(Alley.Lights))
Summary.Table.Alley[1,2]  <- 30*sum(Alley.Lights$Crimes.All.During)       /sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[2,2]  <- 30*sum(Alley.Lights$Theft.During)           /sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[3,2]  <- 30*sum(Alley.Lights$Narcotics.During)        /sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[4,2]  <- 30*sum(Alley.Lights$Battery.During)          /sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[5,2]  <- 30*sum(Alley.Lights$CriminalDamage.During)   /sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[6,2]  <- 30*sum(Alley.Lights$MotorVehicleTheft.During)/sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[7,2]  <- 30*sum(Alley.Lights$Robbery.During)          /sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[8,2]  <- 30*sum(Alley.Lights$Assault.During)          /sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[9,2]  <- 30*sum(Alley.Lights$Burglary.During)         /sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[10,2] <- 30*sum(Alley.Lights$Homicide.During)         /sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[11,2] <- 30*sum(Alley.Lights$DeceptivePractice.During)/sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[1,3]  <- 30*sum(Alley.Lights$Crimes.All.After)        /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[2,3]  <- 30*sum(Alley.Lights$Theft.After)            /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[3,3]  <- 30*sum(Alley.Lights$Narcotics.After)         /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[4,3]  <- 30*sum(Alley.Lights$Battery.After)           /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[5,3]  <- 30*sum(Alley.Lights$CriminalDamage.After)    /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[6,3]  <- 30*sum(Alley.Lights$MotorVehicleTheft.After) /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[7,3]  <- 30*sum(Alley.Lights$Robbery.After)           /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[8,3]  <- 30*sum(Alley.Lights$Assault.After)           /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[9,3]  <- 30*sum(Alley.Lights$Burglary.After)          /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[10,3] <- 30*sum(Alley.Lights$Homicide.After)          /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[11,3] <- 30*sum(Alley.Lights$DeceptivePractice.After) /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[1,4]  <- 30*(sum(Alley.Lights$Crimes.All.Before)       +sum(Alley.Lights$Crimes.All.After))       /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[2,4]  <- 30*(sum(Alley.Lights$Theft.Before)            +sum(Alley.Lights$Theft.After))           /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[3,4]  <- 30*(sum(Alley.Lights$Narcotics.Before)        +sum(Alley.Lights$Narcotics.After))        /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[4,4]  <- 30*(sum(Alley.Lights$Battery.Before)          +sum(Alley.Lights$Battery.After))          /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[5,4]  <- 30*(sum(Alley.Lights$CriminalDamage.Before)   +sum(Alley.Lights$CriminalDamage.After))   /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[6,4]  <- 30*(sum(Alley.Lights$MotorVehicleTheft.Before)+sum(Alley.Lights$MotorVehicleTheft.After))/(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[7,4]  <- 30*(sum(Alley.Lights$Robbery.Before)          +sum(Alley.Lights$Robbery.After))          /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[8,4]  <- 30*(sum(Alley.Lights$Assault.Before)          +sum(Alley.Lights$Assault.After))          /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[9,4]  <- 30*(sum(Alley.Lights$Burglary.Before)         +sum(Alley.Lights$Burglary.After))         /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[10,4] <- 30*(sum(Alley.Lights$Homicide.Before)         +sum(Alley.Lights$Homicide.After))         /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[11,4] <- 30*(sum(Alley.Lights$DeceptivePractice.Before)+sum(Alley.Lights$DeceptivePractice.After))/(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))


Summary.Table.OneOut[1,1]  <- 30*sum(Street.Lights.OneOut$Crimes.All.Before)       /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[2,1]  <- 30*sum(Street.Lights.OneOut$Theft.Before)           /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[3,1]  <- 30*sum(Street.Lights.OneOut$Narcotics.Before)        /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[4,1]  <- 30*sum(Street.Lights.OneOut$Battery.Before)          /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[5,1]  <- 30*sum(Street.Lights.OneOut$CriminalDamage.Before)   /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[6,1]  <- 30*sum(Street.Lights.OneOut$MotorVehicleTheft.Before)/(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[7,1]  <- 30*sum(Street.Lights.OneOut$Robbery.Before)          /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[8,1] <- 30*sum(Street.Lights.OneOut$Assault.Before)           /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[9,1] <- 30*sum(Street.Lights.OneOut$Burglary.Before)          /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[10,1] <- 30*sum(Street.Lights.OneOut$Homicide.Before)         /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[11,1] <- 30*sum(Street.Lights.OneOut$DeceptivePractice.Before)/(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[1,2]  <- 30*sum(Street.Lights.OneOut$Crimes.All.During)       /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[2,2]  <- 30*sum(Street.Lights.OneOut$Theft.During)           /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[3,2]  <- 30*sum(Street.Lights.OneOut$Narcotics.During)        /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[4,2]  <- 30*sum(Street.Lights.OneOut$Battery.During)          /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[5,2]  <- 30*sum(Street.Lights.OneOut$CriminalDamage.During)   /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[6,2]  <- 30*sum(Street.Lights.OneOut$MotorVehicleTheft.During)/sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[7,2]  <- 30*sum(Street.Lights.OneOut$Robbery.During)          /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[8,2] <- 30*sum(Street.Lights.OneOut$Assault.During)           /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[9,2] <- 30*sum(Street.Lights.OneOut$Burglary.During)          /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[10,2] <- 30*sum(Street.Lights.OneOut$Homicide.During)         /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[11,2] <- 30*sum(Street.Lights.OneOut$DeceptivePractice.During)/sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[1,3]  <- 30*sum(Street.Lights.OneOut$Crimes.All.After)        /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[2,3]  <- 30*sum(Street.Lights.OneOut$Theft.After)            /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[3,3]  <- 30*sum(Street.Lights.OneOut$Narcotics.After)         /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[4,3]  <- 30*sum(Street.Lights.OneOut$Battery.After)           /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[5,3]  <- 30*sum(Street.Lights.OneOut$CriminalDamage.After)    /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[6,3]  <- 30*sum(Street.Lights.OneOut$MotorVehicleTheft.After) /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[7,3]  <- 30*sum(Street.Lights.OneOut$Robbery.After)           /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[8,3]  <- 30*sum(Street.Lights.OneOut$Assault.After)           /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[9,3]  <- 30*sum(Street.Lights.OneOut$Burglary.After)          /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[10,3] <- 30*sum(Street.Lights.OneOut$Homicide.After)          /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[11,3] <- 30*sum(Street.Lights.OneOut$DeceptivePractice.After) /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[1,4]  <- 30*(sum(Street.Lights.OneOut$Crimes.All.Before)       +sum(Street.Lights.OneOut$Crimes.All.After))       /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[2,4]  <- 30*(sum(Street.Lights.OneOut$Theft.Before)           +sum(Street.Lights.OneOut$Theft.After))           /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[3,4]  <- 30*(sum(Street.Lights.OneOut$Narcotics.Before)        +sum(Street.Lights.OneOut$Narcotics.After))        /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[4,4]  <- 30*(sum(Street.Lights.OneOut$Battery.Before)          +sum(Street.Lights.OneOut$Battery.After))          /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[5,4]  <- 30*(sum(Street.Lights.OneOut$CriminalDamage.Before)   +sum(Street.Lights.OneOut$CriminalDamage.After))   /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[6,4]  <- 30*(sum(Street.Lights.OneOut$MotorVehicleTheft.Before)+sum(Street.Lights.OneOut$MotorVehicleTheft.After))/(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[7,4]  <- 30*(sum(Street.Lights.OneOut$Robbery.Before)          +sum(Street.Lights.OneOut$Robbery.After))          /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[8,4]  <- 30*(sum(Street.Lights.OneOut$Assault.Before)           +sum(Street.Lights.OneOut$Assault.After))          /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[9,4]  <- 30*(sum(Street.Lights.OneOut$Burglary.Before)          +sum(Street.Lights.OneOut$Burglary.After))         /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[10,4] <- 30*(sum(Street.Lights.OneOut$Homicide.Before)         +sum(Street.Lights.OneOut$Homicide.After))         /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[11,4] <- 30*(sum(Street.Lights.OneOut$DeceptivePractice.Before)+sum(Street.Lights.OneOut$DeceptivePractice.After))/(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))



Summary.Table.AllOut[1,1]  <- 30*sum(Street.Lights.AllOut$Crimes.All.Before)       /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[2,1]  <- 30*sum(Street.Lights.AllOut$Theft.Before)           /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[3,1]  <- 30*sum(Street.Lights.AllOut$Narcotics.Before)        /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[4,1]  <- 30*sum(Street.Lights.AllOut$Battery.Before)          /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[5,1]  <- 30*sum(Street.Lights.AllOut$CriminalDamage.Before)   /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[6,1]  <- 30*sum(Street.Lights.AllOut$MotorVehicleTheft.Before)/(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[7,1]  <- 30*sum(Street.Lights.AllOut$Robbery.Before)          /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[8,1]  <- 30*sum(Street.Lights.AllOut$Assault.Before)          /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[9,1]  <- 30*sum(Street.Lights.AllOut$Burglary.Before)         /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[10,1] <- 30*sum(Street.Lights.AllOut$Homicide.Before)         /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[11,1] <- 30*sum(Street.Lights.AllOut$DeceptivePractice.Before)/(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[1,2]  <- 30*sum(Street.Lights.AllOut$Crimes.All.During)       /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[2,2]  <- 30*sum(Street.Lights.AllOut$Theft.During)           /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[3,2]  <- 30*sum(Street.Lights.AllOut$Narcotics.During)        /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[4,2]  <- 30*sum(Street.Lights.AllOut$Battery.During)          /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[5,2]  <- 30*sum(Street.Lights.AllOut$CriminalDamage.During)   /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[6,2]  <- 30*sum(Street.Lights.AllOut$MotorVehicleTheft.During)/sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[7,2]  <- 30*sum(Street.Lights.AllOut$Robbery.During)          /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[8,2]  <- 30*sum(Street.Lights.AllOut$Assault.During)          /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[9,2]  <- 30*sum(Street.Lights.AllOut$Burglary.During)         /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[10,2] <- 30*sum(Street.Lights.AllOut$Homicide.During)         /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[11,2] <- 30*sum(Street.Lights.AllOut$DeceptivePractice.During)/sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[1,3]  <- 30*sum(Street.Lights.AllOut$Crimes.All.After)        /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[2,3]  <- 30*sum(Street.Lights.AllOut$Theft.After)            /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[3,3]  <- 30*sum(Street.Lights.AllOut$Narcotics.After)         /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[4,3]  <- 30*sum(Street.Lights.AllOut$Battery.After)           /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[5,3]  <- 30*sum(Street.Lights.AllOut$CriminalDamage.After)    /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[6,3]  <- 30*sum(Street.Lights.AllOut$MotorVehicleTheft.After) /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[7,3]  <- 30*sum(Street.Lights.AllOut$Robbery.After)           /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[8,3]  <- 30*sum(Street.Lights.AllOut$Assault.After)           /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[9,3]  <- 30*sum(Street.Lights.AllOut$Burglary.After)          /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[10,3] <- 30*sum(Street.Lights.AllOut$Homicide.After)          /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[11,3] <- 30*sum(Street.Lights.AllOut$DeceptivePractice.After) /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[1,4]  <- 30*(sum(Street.Lights.AllOut$Crimes.All.Before)       +sum(Street.Lights.AllOut$Crimes.All.After))       /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[2,4]  <- 30*(sum(Street.Lights.AllOut$Theft.Before)            +sum(Street.Lights.AllOut$Theft.After))            /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[3,4]  <- 30*(sum(Street.Lights.AllOut$Narcotics.Before)        +sum(Street.Lights.AllOut$Narcotics.After))        /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[4,4]  <- 30*(sum(Street.Lights.AllOut$Battery.Before)          +sum(Street.Lights.AllOut$Battery.After))          /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[5,4]  <- 30*(sum(Street.Lights.AllOut$CriminalDamage.Before)   +sum(Street.Lights.AllOut$CriminalDamage.After))   /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[6,4]  <- 30*(sum(Street.Lights.AllOut$MotorVehicleTheft.Before)+sum(Street.Lights.AllOut$MotorVehicleTheft.After))/(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[7,4]  <- 30*(sum(Street.Lights.AllOut$Robbery.Before)          +sum(Street.Lights.AllOut$Robbery.After))          /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[8,4]  <- 30*(sum(Street.Lights.AllOut$Assault.Before)          +sum(Street.Lights.AllOut$Assault.After))          /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[9,4]  <- 30*(sum(Street.Lights.AllOut$Burglary.Before)         +sum(Street.Lights.AllOut$Burglary.After))         /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[10,4] <- 30*(sum(Street.Lights.AllOut$Homicide.Before)         +sum(Street.Lights.AllOut$Homicide.After))         /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[11,4] <- 30*(sum(Street.Lights.AllOut$DeceptivePractice.Before)+sum(Street.Lights.AllOut$DeceptivePractice.After))/(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))

Summary.Table.Alley$AbsDiff  <- Summary.Table.Alley$During  - Summary.Table.Alley$AvgBeforeAfter
Summary.Table.OneOut$AbsDiff <- Summary.Table.OneOut$During - Summary.Table.OneOut$AvgBeforeAfter
Summary.Table.AllOut$AbsDiff <- Summary.Table.AllOut$During - Summary.Table.AllOut$AvgBeforeAfter

Summary.Table.Alley$PctDiff  <- 100*Summary.Table.Alley$AbsDiff /Summary.Table.Alley$AvgBeforeAfter
Summary.Table.OneOut$PctDiff <- 100*Summary.Table.OneOut$AbsDiff/Summary.Table.OneOut$AvgBeforeAfter
Summary.Table.AllOut$PctDiff <- 100*Summary.Table.AllOut$AbsDiff/Summary.Table.AllOut$AvgBeforeAfter



#Create Datasets Formatted for Poisson Modeling and Estimation of SE's
Alley.Lights.Pois <- Alley.Lights[rep(seq(nrow(Alley.Lights)),3),]
Alley.Lights.Pois <- Alley.Lights.Pois[order(Alley.Lights.Pois$Service.Request.No),]
Alley.Lights.Pois$Time              <- rep(c("BEFORE", "DURING", "AFTER"), nrow(Alley.Lights))
Alley.Lights.Pois$AllCrimes         <- numeric(nrow(Alley.Lights.Pois))
Alley.Lights.Pois$Theft             <- numeric(nrow(Alley.Lights.Pois))
Alley.Lights.Pois$Narcotics         <- numeric(nrow(Alley.Lights.Pois))
Alley.Lights.Pois$Battery           <- numeric(nrow(Alley.Lights.Pois))
Alley.Lights.Pois$CriminalDamage    <- numeric(nrow(Alley.Lights.Pois))
Alley.Lights.Pois$MotorVehicleTheft <- numeric(nrow(Alley.Lights.Pois))
Alley.Lights.Pois$Robbery           <- numeric(nrow(Alley.Lights.Pois))
Alley.Lights.Pois$Assault           <- numeric(nrow(Alley.Lights.Pois))
Alley.Lights.Pois$Burglary          <- numeric(nrow(Alley.Lights.Pois))
Alley.Lights.Pois$Homicide          <- numeric(nrow(Alley.Lights.Pois))
Alley.Lights.Pois$DeceptivePractice <- numeric(nrow(Alley.Lights.Pois))
Alley.Lights.Pois$Duration          <- numeric(nrow(Alley.Lights.Pois))
Alley.Lights.Pois$AllCrimes[which(Alley.Lights.Pois$Time=="BEFORE")]         <- Alley.Lights.Pois$Crimes.All.Before[which(Alley.Lights.Pois$Time=="BEFORE")]
Alley.Lights.Pois$Theft[which(Alley.Lights.Pois$Time=="BEFORE")]            <- Alley.Lights.Pois$Theft.Before[which(Alley.Lights.Pois$Time=="BEFORE")]
Alley.Lights.Pois$Narcotics[which(Alley.Lights.Pois$Time=="BEFORE")]         <- Alley.Lights.Pois$Narcotics.Before[which(Alley.Lights.Pois$Time=="BEFORE")]
Alley.Lights.Pois$Battery[which(Alley.Lights.Pois$Time=="BEFORE")]           <- Alley.Lights.Pois$Battery.Before[which(Alley.Lights.Pois$Time=="BEFORE")]
Alley.Lights.Pois$CriminalDamage[which(Alley.Lights.Pois$Time=="BEFORE")]    <- Alley.Lights.Pois$CriminalDamage.Before[which(Alley.Lights.Pois$Time=="BEFORE")]
Alley.Lights.Pois$MotorVehicleTheft[which(Alley.Lights.Pois$Time=="BEFORE")] <- Alley.Lights.Pois$MotorVehicleTheft.Before[which(Alley.Lights.Pois$Time=="BEFORE")]
Alley.Lights.Pois$Robbery[which(Alley.Lights.Pois$Time=="BEFORE")]           <- Alley.Lights.Pois$Robbery.Before[which(Alley.Lights.Pois$Time=="BEFORE")]
Alley.Lights.Pois$Assault[which(Alley.Lights.Pois$Time=="BEFORE")]           <- Alley.Lights.Pois$Assault.Before[which(Alley.Lights.Pois$Time=="BEFORE")]
Alley.Lights.Pois$Burglary[which(Alley.Lights.Pois$Time=="BEFORE")]          <- Alley.Lights.Pois$Burglary.Before[which(Alley.Lights.Pois$Time=="BEFORE")]
Alley.Lights.Pois$Homicide[which(Alley.Lights.Pois$Time=="BEFORE")]          <- Alley.Lights.Pois$Homicide.Before[which(Alley.Lights.Pois$Time=="BEFORE")]
Alley.Lights.Pois$DeceptivePractice[which(Alley.Lights.Pois$Time=="BEFORE")] <- Alley.Lights.Pois$DeceptivePractice.Before[which(Alley.Lights.Pois$Time=="BEFORE")]
Alley.Lights.Pois$Duration[which(Alley.Lights.Pois$Time=="BEFORE")]          <- rep(30, length(which(Alley.Lights.Pois$Time=="BEFORE")))

Alley.Lights.Pois$AllCrimes[which(Alley.Lights.Pois$Time=="DURING")]         <- Alley.Lights.Pois$Crimes.All.During[which(Alley.Lights.Pois$Time=="DURING")]
Alley.Lights.Pois$Theft[which(Alley.Lights.Pois$Time=="DURING")]            <- Alley.Lights.Pois$Theft.During[which(Alley.Lights.Pois$Time=="DURING")]
Alley.Lights.Pois$Narcotics[which(Alley.Lights.Pois$Time=="DURING")]         <- Alley.Lights.Pois$Narcotics.During[which(Alley.Lights.Pois$Time=="DURING")]
Alley.Lights.Pois$Battery[which(Alley.Lights.Pois$Time=="DURING")]           <- Alley.Lights.Pois$Battery.During[which(Alley.Lights.Pois$Time=="DURING")]
Alley.Lights.Pois$CriminalDamage[which(Alley.Lights.Pois$Time=="DURING")]    <- Alley.Lights.Pois$CriminalDamage.During[which(Alley.Lights.Pois$Time=="DURING")]
Alley.Lights.Pois$MotorVehicleTheft[which(Alley.Lights.Pois$Time=="DURING")] <- Alley.Lights.Pois$MotorVehicleTheft.During[which(Alley.Lights.Pois$Time=="DURING")]
Alley.Lights.Pois$Robbery[which(Alley.Lights.Pois$Time=="DURING")]           <- Alley.Lights.Pois$Robbery.During[which(Alley.Lights.Pois$Time=="DURING")]
Alley.Lights.Pois$Assault[which(Alley.Lights.Pois$Time=="DURING")]           <- Alley.Lights.Pois$Assault.During[which(Alley.Lights.Pois$Time=="DURING")]
Alley.Lights.Pois$Burglary[which(Alley.Lights.Pois$Time=="DURING")]          <- Alley.Lights.Pois$Burglary.During[which(Alley.Lights.Pois$Time=="DURING")]
Alley.Lights.Pois$Homicide[which(Alley.Lights.Pois$Time=="DURING")]          <- Alley.Lights.Pois$Homicide.During[which(Alley.Lights.Pois$Time=="DURING")]
Alley.Lights.Pois$DeceptivePractice[which(Alley.Lights.Pois$Time=="DURING")] <- Alley.Lights.Pois$DeceptivePractice.During[which(Alley.Lights.Pois$Time=="DURING")]
Alley.Lights.Pois$Duration[which(Alley.Lights.Pois$Time=="DURING")]          <- Alley.Lights.Pois$OutageDuration[which(Alley.Lights.Pois$Time=="DURING")]

Alley.Lights.Pois$AllCrimes[which(Alley.Lights.Pois$Time=="AFTER")]         <- Alley.Lights.Pois$Crimes.All.After[which(Alley.Lights.Pois$Time=="AFTER")]
Alley.Lights.Pois$Theft[which(Alley.Lights.Pois$Time=="AFTER")]            <- Alley.Lights.Pois$Theft.After[which(Alley.Lights.Pois$Time=="AFTER")]
Alley.Lights.Pois$Narcotics[which(Alley.Lights.Pois$Time=="AFTER")]         <- Alley.Lights.Pois$Narcotics.After[which(Alley.Lights.Pois$Time=="AFTER")]
Alley.Lights.Pois$Battery[which(Alley.Lights.Pois$Time=="AFTER")]           <- Alley.Lights.Pois$Battery.After[which(Alley.Lights.Pois$Time=="AFTER")]
Alley.Lights.Pois$CriminalDamage[which(Alley.Lights.Pois$Time=="AFTER")]    <- Alley.Lights.Pois$CriminalDamage.After[which(Alley.Lights.Pois$Time=="AFTER")]
Alley.Lights.Pois$MotorVehicleTheft[which(Alley.Lights.Pois$Time=="AFTER")] <- Alley.Lights.Pois$MotorVehicleTheft.After[which(Alley.Lights.Pois$Time=="AFTER")]
Alley.Lights.Pois$Robbery[which(Alley.Lights.Pois$Time=="AFTER")]           <- Alley.Lights.Pois$Robbery.After[which(Alley.Lights.Pois$Time=="AFTER")]
Alley.Lights.Pois$Assault[which(Alley.Lights.Pois$Time=="AFTER")]           <- Alley.Lights.Pois$Assault.After[which(Alley.Lights.Pois$Time=="AFTER")]
Alley.Lights.Pois$Burglary[which(Alley.Lights.Pois$Time=="AFTER")]          <- Alley.Lights.Pois$Burglary.After[which(Alley.Lights.Pois$Time=="AFTER")]
Alley.Lights.Pois$Homicide[which(Alley.Lights.Pois$Time=="AFTER")]          <- Alley.Lights.Pois$Homicide.After[which(Alley.Lights.Pois$Time=="AFTER")]
Alley.Lights.Pois$DeceptivePractice[which(Alley.Lights.Pois$Time=="AFTER")] <- Alley.Lights.Pois$DeceptivePractice.After[which(Alley.Lights.Pois$Time=="AFTER")]
Alley.Lights.Pois$Duration[which(Alley.Lights.Pois$Time=="AFTER")]          <- Alley.Lights.Pois$After.Period.Duration[which(Alley.Lights.Pois$Time=="AFTER")]

Alley.Lights.Pois <- Alley.Lights.Pois[,c("Type", "Service.Request.No","DateCreated", "DateCompleted", "Location", "Outcome", "x_coord", "y_coord", "zip_code", "ward",
                                           "police_district", "community_area", "Time", "AllCrimes", "Theft", "Narcotics", "Battery", "CriminalDamage", 
                                           "MotorVehicleTheft", "Robbery", "Assault", "Burglary", "Homicide", "DeceptivePractice", "Duration")]
Alley.Lights.Pois$OutageInd <- numeric(nrow(Alley.Lights.Pois))
Alley.Lights.Pois$OutageInd[which(Alley.Lights.Pois$Time=="DURING")] <- rep(1, length(which(Alley.Lights.Pois$Time=="DURING")))

Fit.Alley.AllCrimes         <- glm(AllCrimes         ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Theft            <- glm(Theft            ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Narcotics         <- glm(Narcotics         ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Battery           <- glm(Battery           ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.CriminalDamage    <- glm(CriminalDamage    ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.MotorVehicleTheft <- glm(MotorVehicleTheft ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Robbery           <- glm(Robbery           ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Assault           <- glm(Assault           ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Burglary          <- glm(Burglary          ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Homicide          <- glm(Homicide          ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.DeceptivePractice <- glm(DeceptivePractice ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Alley.Lights.Pois)
Summary.Table.Alley$LL <- numeric(nrow(Summary.Table.Alley))
Summary.Table.Alley$UL <- numeric(nrow(Summary.Table.Alley))
Summary.Table.Alley$p.value <- numeric(nrow(Summary.Table.Alley))
Summary.Table.Alley$star <- character(nrow(Summary.Table.Alley))
Summary.Table.Alley$p.value[1]  <- summary(Fit.Alley.AllCrimes)$coef[2,4]
Summary.Table.Alley$p.value[2]  <- summary(Fit.Alley.Theft)$coef[2,4]
Summary.Table.Alley$p.value[3]  <- summary(Fit.Alley.Narcotics)$coef[2,4]
Summary.Table.Alley$p.value[4]  <- summary(Fit.Alley.Battery)$coef[2,4]
Summary.Table.Alley$p.value[5]  <- summary(Fit.Alley.CriminalDamage)$coef[2,4]
Summary.Table.Alley$p.value[6]  <- summary(Fit.Alley.MotorVehicleTheft)$coef[2,4]
Summary.Table.Alley$p.value[7]  <- summary(Fit.Alley.Robbery)$coef[2,4]
Summary.Table.Alley$p.value[8]  <- summary(Fit.Alley.Assault)$coef[2,4]
Summary.Table.Alley$p.value[9]  <- summary(Fit.Alley.Burglary)$coef[2,4]
Summary.Table.Alley$p.value[10] <- summary(Fit.Alley.Homicide)$coef[2,4]
Summary.Table.Alley$p.value[11] <- summary(Fit.Alley.DeceptivePractice)$coef[2,4]
Summary.Table.Alley$LL[1]  <- 100*exp(Fit.Alley.AllCrimes$coefficients[2]         - qt(.975, length(Fit.Alley.AllCrimes$residuals-1))        *summary(Fit.Alley.AllCrimes)$coef[2,2])        -100
Summary.Table.Alley$LL[2]  <- 100*exp(Fit.Alley.Theft$coefficients[2]            - qt(.975, length(Fit.Alley.Theft$residuals-1))           *summary(Fit.Alley.Theft)$coef[2,2])           -100
Summary.Table.Alley$LL[3]  <- 100*exp(Fit.Alley.Narcotics$coefficients[2]         - qt(.975, length(Fit.Alley.Narcotics$residuals-1))        *summary(Fit.Alley.Narcotics)$coef[2,2])        -100
Summary.Table.Alley$LL[4]  <- 100*exp(Fit.Alley.Battery$coefficients[2]           - qt(.975, length(Fit.Alley.Battery$residuals-1))          *summary(Fit.Alley.Battery)$coef[2,2])          -100
Summary.Table.Alley$LL[5]  <- 100*exp(Fit.Alley.CriminalDamage$coefficients[2]    - qt(.975, length(Fit.Alley.CriminalDamage$residuals-1))   *summary(Fit.Alley.CriminalDamage)$coef[2,2])   -100
Summary.Table.Alley$LL[6]  <- 100*exp(Fit.Alley.MotorVehicleTheft$coefficients[2] - qt(.975, length(Fit.Alley.MotorVehicleTheft$residuals-1))*summary(Fit.Alley.MotorVehicleTheft)$coef[2,2])-100
Summary.Table.Alley$LL[7]  <- 100*exp(Fit.Alley.Robbery$coefficients[2]           - qt(.975, length(Fit.Alley.Robbery$residuals-1))          *summary(Fit.Alley.Robbery)$coef[2,2])          -100
Summary.Table.Alley$LL[8]  <- 100*exp(Fit.Alley.Assault$coefficients[2]           - qt(.975, length(Fit.Alley.Assault$residuals-1))          *summary(Fit.Alley.Assault)$coef[2,2])          -100
Summary.Table.Alley$LL[9]  <- 100*exp(Fit.Alley.Burglary$coefficients[2]          - qt(.975, length(Fit.Alley.Burglary$residuals-1))         *summary(Fit.Alley.Burglary)$coef[2,2])         -100
Summary.Table.Alley$LL[10] <- 100*exp(Fit.Alley.Homicide$coefficients[2]          - qt(.975, length(Fit.Alley.Homicide$residuals-1))         *summary(Fit.Alley.Homicide)$coef[2,2])         -100
Summary.Table.Alley$LL[11] <- 100*exp(Fit.Alley.DeceptivePractice$coefficients[2] - qt(.975, length(Fit.Alley.DeceptivePractice$residuals-1))*summary(Fit.Alley.DeceptivePractice)$coef[2,2])-100
Summary.Table.Alley$UL[1]  <- 100*exp(Fit.Alley.AllCrimes$coefficients[2]         + qt(.975, length(Fit.Alley.AllCrimes$residuals-1))        *summary(Fit.Alley.AllCrimes)$coef[2,2])        -100
Summary.Table.Alley$UL[2]  <- 100*exp(Fit.Alley.Theft$coefficients[2]            + qt(.975, length(Fit.Alley.Theft$residuals-1))           *summary(Fit.Alley.Theft)$coef[2,2])           -100
Summary.Table.Alley$UL[3]  <- 100*exp(Fit.Alley.Narcotics$coefficients[2]         + qt(.975, length(Fit.Alley.Narcotics$residuals-1))        *summary(Fit.Alley.Narcotics)$coef[2,2])        -100
Summary.Table.Alley$UL[4]  <- 100*exp(Fit.Alley.Battery$coefficients[2]           + qt(.975, length(Fit.Alley.Battery$residuals-1))          *summary(Fit.Alley.Battery)$coef[2,2])          -100
Summary.Table.Alley$UL[5]  <- 100*exp(Fit.Alley.CriminalDamage$coefficients[2]    + qt(.975, length(Fit.Alley.CriminalDamage$residuals-1))   *summary(Fit.Alley.CriminalDamage)$coef[2,2])   -100
Summary.Table.Alley$UL[6]  <- 100*exp(Fit.Alley.MotorVehicleTheft$coefficients[2] + qt(.975, length(Fit.Alley.MotorVehicleTheft$residuals-1))*summary(Fit.Alley.MotorVehicleTheft)$coef[2,2])-100
Summary.Table.Alley$UL[7]  <- 100*exp(Fit.Alley.Robbery$coefficients[2]           + qt(.975, length(Fit.Alley.Robbery$residuals-1))          *summary(Fit.Alley.Robbery)$coef[2,2])          -100
Summary.Table.Alley$UL[8]  <- 100*exp(Fit.Alley.Assault$coefficients[2]           + qt(.975, length(Fit.Alley.Assault$residuals-1))          *summary(Fit.Alley.Assault)$coef[2,2])          -100
Summary.Table.Alley$UL[9]  <- 100*exp(Fit.Alley.Burglary$coefficients[2]          + qt(.975, length(Fit.Alley.Burglary$residuals-1))         *summary(Fit.Alley.Burglary)$coef[2,2])         -100
Summary.Table.Alley$UL[10] <- 100*exp(Fit.Alley.Homicide$coefficients[2]          + qt(.975, length(Fit.Alley.Homicide$residuals-1))         *summary(Fit.Alley.Homicide)$coef[2,2])         -100
Summary.Table.Alley$UL[11] <- 100*exp(Fit.Alley.DeceptivePractice$coefficients[2] + qt(.975, length(Fit.Alley.DeceptivePractice$residuals-1))*summary(Fit.Alley.DeceptivePractice)$coef[2,2])         -100
Summary.Table.Alley$star[which(Summary.Table.Alley$p.value<0.01)]                                     <- rep("**", length(which(Summary.Table.Alley$p.value<0.01)))
Summary.Table.Alley$star[which(Summary.Table.Alley$p.value<0.05 & Summary.Table.Alley$p.value>=0.01)] <- rep("*" , length(which(Summary.Table.Alley$p.value<0.05 & Summary.Table.Alley$p.value>=0.01)))




Street.Lights.OneOut.Pois <- Street.Lights.OneOut[rep(seq(nrow(Street.Lights.OneOut)),3),]
Street.Lights.OneOut.Pois <- Street.Lights.OneOut.Pois[order(Street.Lights.OneOut.Pois$Service.Request.No),]
Street.Lights.OneOut.Pois$Time              <- rep(c("BEFORE", "DURING", "AFTER"), nrow(Street.Lights.OneOut))
Street.Lights.OneOut.Pois$AllCrimes         <- numeric(nrow(Street.Lights.OneOut.Pois))
Street.Lights.OneOut.Pois$Theft            <- numeric(nrow(Street.Lights.OneOut.Pois))
Street.Lights.OneOut.Pois$Narcotics         <- numeric(nrow(Street.Lights.OneOut.Pois))
Street.Lights.OneOut.Pois$Battery           <- numeric(nrow(Street.Lights.OneOut.Pois))
Street.Lights.OneOut.Pois$CriminalDamage    <- numeric(nrow(Street.Lights.OneOut.Pois))
Street.Lights.OneOut.Pois$MotorVehicleTheft <- numeric(nrow(Street.Lights.OneOut.Pois))
Street.Lights.OneOut.Pois$Robbery           <- numeric(nrow(Street.Lights.OneOut.Pois))
Street.Lights.OneOut.Pois$Assault           <- numeric(nrow(Street.Lights.OneOut.Pois))
Street.Lights.OneOut.Pois$Burglary          <- numeric(nrow(Street.Lights.OneOut.Pois))
Street.Lights.OneOut.Pois$Homicide          <- numeric(nrow(Street.Lights.OneOut.Pois))
Street.Lights.OneOut.Pois$DeceptivePractice <- numeric(nrow(Street.Lights.OneOut.Pois))
Street.Lights.OneOut.Pois$Duration          <- numeric(nrow(Street.Lights.OneOut.Pois))
Street.Lights.OneOut.Pois$AllCrimes[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]         <- Street.Lights.OneOut.Pois$Crimes.All.Before[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]
Street.Lights.OneOut.Pois$Theft[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]            <- Street.Lights.OneOut.Pois$Theft.Before[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]
Street.Lights.OneOut.Pois$Narcotics[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]         <- Street.Lights.OneOut.Pois$Narcotics.Before[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]
Street.Lights.OneOut.Pois$Battery[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]           <- Street.Lights.OneOut.Pois$Battery.Before[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]
Street.Lights.OneOut.Pois$CriminalDamage[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]    <- Street.Lights.OneOut.Pois$CriminalDamage.Before[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]
Street.Lights.OneOut.Pois$MotorVehicleTheft[which(Street.Lights.OneOut.Pois$Time=="BEFORE")] <- Street.Lights.OneOut.Pois$MotorVehicleTheft.Before[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]
Street.Lights.OneOut.Pois$Robbery[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]           <- Street.Lights.OneOut.Pois$Robbery.Before[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]
Street.Lights.OneOut.Pois$Assault[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]           <- Street.Lights.OneOut.Pois$Assault.Before[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]
Street.Lights.OneOut.Pois$Burglary[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]          <- Street.Lights.OneOut.Pois$Burglary.Before[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]
Street.Lights.OneOut.Pois$Homicide[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]          <- Street.Lights.OneOut.Pois$Homicide.Before[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]
Street.Lights.OneOut.Pois$DeceptivePractice[which(Street.Lights.OneOut.Pois$Time=="BEFORE")] <- Street.Lights.OneOut.Pois$DeceptivePractice.Before[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]
Street.Lights.OneOut.Pois$Duration[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]          <- rep(30, length(which(Street.Lights.OneOut.Pois$Time=="BEFORE")))

Street.Lights.OneOut.Pois$AllCrimes[which(Street.Lights.OneOut.Pois$Time=="DURING")]         <- Street.Lights.OneOut.Pois$Crimes.All.During[which(Street.Lights.OneOut.Pois$Time=="DURING")]
Street.Lights.OneOut.Pois$Theft[which(Street.Lights.OneOut.Pois$Time=="DURING")]            <- Street.Lights.OneOut.Pois$Theft.During[which(Street.Lights.OneOut.Pois$Time=="DURING")]
Street.Lights.OneOut.Pois$Narcotics[which(Street.Lights.OneOut.Pois$Time=="DURING")]         <- Street.Lights.OneOut.Pois$Narcotics.During[which(Street.Lights.OneOut.Pois$Time=="DURING")]
Street.Lights.OneOut.Pois$Battery[which(Street.Lights.OneOut.Pois$Time=="DURING")]           <- Street.Lights.OneOut.Pois$Battery.During[which(Street.Lights.OneOut.Pois$Time=="DURING")]
Street.Lights.OneOut.Pois$CriminalDamage[which(Street.Lights.OneOut.Pois$Time=="DURING")]    <- Street.Lights.OneOut.Pois$CriminalDamage.During[which(Street.Lights.OneOut.Pois$Time=="DURING")]
Street.Lights.OneOut.Pois$MotorVehicleTheft[which(Street.Lights.OneOut.Pois$Time=="DURING")] <- Street.Lights.OneOut.Pois$MotorVehicleTheft.During[which(Street.Lights.OneOut.Pois$Time=="DURING")]
Street.Lights.OneOut.Pois$Robbery[which(Street.Lights.OneOut.Pois$Time=="DURING")]           <- Street.Lights.OneOut.Pois$Robbery.During[which(Street.Lights.OneOut.Pois$Time=="DURING")]
Street.Lights.OneOut.Pois$Assault[which(Street.Lights.OneOut.Pois$Time=="DURING")]           <- Street.Lights.OneOut.Pois$Assault.During[which(Street.Lights.OneOut.Pois$Time=="DURING")]
Street.Lights.OneOut.Pois$Burglary[which(Street.Lights.OneOut.Pois$Time=="DURING")]          <- Street.Lights.OneOut.Pois$Burglary.During[which(Street.Lights.OneOut.Pois$Time=="DURING")]
Street.Lights.OneOut.Pois$Homicide[which(Street.Lights.OneOut.Pois$Time=="DURING")]          <- Street.Lights.OneOut.Pois$Homicide.During[which(Street.Lights.OneOut.Pois$Time=="DURING")]
Street.Lights.OneOut.Pois$DeceptivePractice[which(Street.Lights.OneOut.Pois$Time=="DURING")] <- Street.Lights.OneOut.Pois$DeceptivePractice.During[which(Street.Lights.OneOut.Pois$Time=="DURING")]
Street.Lights.OneOut.Pois$Duration[which(Street.Lights.OneOut.Pois$Time=="DURING")]          <- Street.Lights.OneOut.Pois$OutageDuration[which(Street.Lights.OneOut.Pois$Time=="DURING")]

Street.Lights.OneOut.Pois$AllCrimes[which(Street.Lights.OneOut.Pois$Time=="AFTER")]         <- Street.Lights.OneOut.Pois$Crimes.All.After[which(Street.Lights.OneOut.Pois$Time=="AFTER")]
Street.Lights.OneOut.Pois$Theft[which(Street.Lights.OneOut.Pois$Time=="AFTER")]            <- Street.Lights.OneOut.Pois$Theft.After[which(Street.Lights.OneOut.Pois$Time=="AFTER")]
Street.Lights.OneOut.Pois$Narcotics[which(Street.Lights.OneOut.Pois$Time=="AFTER")]         <- Street.Lights.OneOut.Pois$Narcotics.After[which(Street.Lights.OneOut.Pois$Time=="AFTER")]
Street.Lights.OneOut.Pois$Battery[which(Street.Lights.OneOut.Pois$Time=="AFTER")]           <- Street.Lights.OneOut.Pois$Battery.After[which(Street.Lights.OneOut.Pois$Time=="AFTER")]
Street.Lights.OneOut.Pois$CriminalDamage[which(Street.Lights.OneOut.Pois$Time=="AFTER")]    <- Street.Lights.OneOut.Pois$CriminalDamage.After[which(Street.Lights.OneOut.Pois$Time=="AFTER")]
Street.Lights.OneOut.Pois$MotorVehicleTheft[which(Street.Lights.OneOut.Pois$Time=="AFTER")] <- Street.Lights.OneOut.Pois$MotorVehicleTheft.After[which(Street.Lights.OneOut.Pois$Time=="AFTER")]
Street.Lights.OneOut.Pois$Robbery[which(Street.Lights.OneOut.Pois$Time=="AFTER")]           <- Street.Lights.OneOut.Pois$Robbery.After[which(Street.Lights.OneOut.Pois$Time=="AFTER")]
Street.Lights.OneOut.Pois$Assault[which(Street.Lights.OneOut.Pois$Time=="AFTER")]           <- Street.Lights.OneOut.Pois$Assault.After[which(Street.Lights.OneOut.Pois$Time=="AFTER")]
Street.Lights.OneOut.Pois$Burglary[which(Street.Lights.OneOut.Pois$Time=="AFTER")]          <- Street.Lights.OneOut.Pois$Burglary.After[which(Street.Lights.OneOut.Pois$Time=="AFTER")]
Street.Lights.OneOut.Pois$Homicide[which(Street.Lights.OneOut.Pois$Time=="AFTER")]          <- Street.Lights.OneOut.Pois$Homicide.After[which(Street.Lights.OneOut.Pois$Time=="AFTER")]
Street.Lights.OneOut.Pois$DeceptivePractice[which(Street.Lights.OneOut.Pois$Time=="AFTER")] <- Street.Lights.OneOut.Pois$DeceptivePractice.After[which(Street.Lights.OneOut.Pois$Time=="AFTER")]
Street.Lights.OneOut.Pois$Duration[which(Street.Lights.OneOut.Pois$Time=="AFTER")]          <- Street.Lights.OneOut.Pois$After.Period.Duration[which(Street.Lights.OneOut.Pois$Time=="AFTER")]

Street.Lights.OneOut.Pois <- Street.Lights.OneOut.Pois[,c("Type", "Service.Request.No","DateCreated", "DateCompleted", "Location", "Outcome", "x_coord", "y_coord", "zip_code", "ward",
                                          "police_district", "community_area", "Time", "AllCrimes", "Theft", "Narcotics", "Battery", "CriminalDamage", 
                                          "MotorVehicleTheft", "Robbery", "Assault", "Burglary", "Homicide", "DeceptivePractice", "Duration")]
Street.Lights.OneOut.Pois$OutageInd <- numeric(nrow(Street.Lights.OneOut.Pois))
Street.Lights.OneOut.Pois$OutageInd[which(Street.Lights.OneOut.Pois$Time=="DURING")] <- rep(1, length(which(Street.Lights.OneOut.Pois$Time=="DURING")))



#Poisson GLM Models to Estimate St. Error of % Diff's
Fit.OneOut.AllCrimes         <- glm(AllCrimes         ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Theft            <- glm(Theft            ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Narcotics         <- glm(Narcotics         ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Battery           <- glm(Battery           ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.CriminalDamage    <- glm(CriminalDamage    ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.MotorVehicleTheft <- glm(MotorVehicleTheft ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Robbery           <- glm(Robbery           ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Assault           <- glm(Assault           ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Burglary          <- glm(Burglary          ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Homicide          <- glm(Homicide          ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.DeceptivePractice <- glm(DeceptivePractice ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Summary.Table.OneOut$LL <- numeric(nrow(Summary.Table.OneOut))
Summary.Table.OneOut$UL <- numeric(nrow(Summary.Table.OneOut))
Summary.Table.OneOut$p.value <- numeric(nrow(Summary.Table.OneOut))
Summary.Table.OneOut$star <- character(nrow(Summary.Table.OneOut))
Summary.Table.OneOut$p.value[1] <- summary(Fit.OneOut.AllCrimes)$coef[2,4]
Summary.Table.OneOut$p.value[2] <- summary(Fit.OneOut.Theft)$coef[2,4]
Summary.Table.OneOut$p.value[3] <- summary(Fit.OneOut.Narcotics)$coef[2,4]
Summary.Table.OneOut$p.value[4] <- summary(Fit.OneOut.Battery)$coef[2,4]
Summary.Table.OneOut$p.value[5] <- summary(Fit.OneOut.CriminalDamage)$coef[2,4]
Summary.Table.OneOut$p.value[6] <- summary(Fit.OneOut.MotorVehicleTheft)$coef[2,4]
Summary.Table.OneOut$p.value[7] <- summary(Fit.OneOut.Robbery)$coef[2,4]
Summary.Table.OneOut$p.value[8] <- summary(Fit.OneOut.Assault)$coef[2,4]
Summary.Table.OneOut$p.value[9] <- summary(Fit.OneOut.Burglary)$coef[2,4]
Summary.Table.OneOut$p.value[10] <- summary(Fit.OneOut.Homicide)$coef[2,4]
Summary.Table.OneOut$p.value[11] <- summary(Fit.OneOut.DeceptivePractice)$coef[2,4]
Summary.Table.OneOut$LL[1]  <- 100*exp(Fit.OneOut.AllCrimes$coefficients[2]         - qt(.975, length(Fit.OneOut.AllCrimes$residuals-1))        *summary(Fit.OneOut.AllCrimes)$coef[2,2])        -100
Summary.Table.OneOut$LL[2]  <- 100*exp(Fit.OneOut.Theft$coefficients[2]            - qt(.975, length(Fit.OneOut.Theft$residuals-1))           *summary(Fit.OneOut.Theft)$coef[2,2])           -100
Summary.Table.OneOut$LL[3]  <- 100*exp(Fit.OneOut.Narcotics$coefficients[2]         - qt(.975, length(Fit.OneOut.Narcotics$residuals-1))        *summary(Fit.OneOut.Narcotics)$coef[2,2])        -100
Summary.Table.OneOut$LL[4]  <- 100*exp(Fit.OneOut.Battery$coefficients[2]           - qt(.975, length(Fit.OneOut.Battery$residuals-1))          *summary(Fit.OneOut.Battery)$coef[2,2])          -100
Summary.Table.OneOut$LL[5]  <- 100*exp(Fit.OneOut.CriminalDamage$coefficients[2]    - qt(.975, length(Fit.OneOut.CriminalDamage$residuals-1))   *summary(Fit.OneOut.CriminalDamage)$coef[2,2])   -100
Summary.Table.OneOut$LL[6]  <- 100*exp(Fit.OneOut.MotorVehicleTheft$coefficients[2] - qt(.975, length(Fit.OneOut.MotorVehicleTheft$residuals-1))*summary(Fit.OneOut.MotorVehicleTheft)$coef[2,2])-100
Summary.Table.OneOut$LL[7]  <- 100*exp(Fit.OneOut.Robbery$coefficients[2]           - qt(.975, length(Fit.OneOut.Robbery$residuals-1))          *summary(Fit.OneOut.Robbery)$coef[2,2])          -100
Summary.Table.OneOut$LL[8]  <- 100*exp(Fit.OneOut.Assault$coefficients[2]           - qt(.975, length(Fit.OneOut.Assault$residuals-1))          *summary(Fit.OneOut.Assault)$coef[2,2])          -100
Summary.Table.OneOut$LL[9]  <- 100*exp(Fit.OneOut.Burglary$coefficients[2]          - qt(.975, length(Fit.OneOut.Burglary$residuals-1))         *summary(Fit.OneOut.Burglary)$coef[2,2])         -100
Summary.Table.OneOut$LL[10] <- 100*exp(Fit.OneOut.Homicide$coefficients[2]          - qt(.975, length(Fit.OneOut.Homicide$residuals-1))         *summary(Fit.OneOut.Homicide)$coef[2,2])         -100
Summary.Table.OneOut$LL[11] <- 100*exp(Fit.OneOut.DeceptivePractice$coefficients[2] - qt(.975, length(Fit.OneOut.DeceptivePractice$residuals-1))*summary(Fit.OneOut.DeceptivePractice)$coef[2,2])-100
Summary.Table.OneOut$UL[1]  <- 100*exp(Fit.OneOut.AllCrimes$coefficients[2]         + qt(.975, length(Fit.OneOut.AllCrimes$residuals-1))        *summary(Fit.OneOut.AllCrimes)$coef[2,2])        -100
Summary.Table.OneOut$UL[2]  <- 100*exp(Fit.OneOut.Theft$coefficients[2]            + qt(.975, length(Fit.OneOut.Theft$residuals-1))           *summary(Fit.OneOut.Theft)$coef[2,2])           -100
Summary.Table.OneOut$UL[3]  <- 100*exp(Fit.OneOut.Narcotics$coefficients[2]         + qt(.975, length(Fit.OneOut.Narcotics$residuals-1))        *summary(Fit.OneOut.Narcotics)$coef[2,2])        -100
Summary.Table.OneOut$UL[4]  <- 100*exp(Fit.OneOut.Battery$coefficients[2]           + qt(.975, length(Fit.OneOut.Battery$residuals-1))          *summary(Fit.OneOut.Battery)$coef[2,2])          -100
Summary.Table.OneOut$UL[5]  <- 100*exp(Fit.OneOut.CriminalDamage$coefficients[2]    + qt(.975, length(Fit.OneOut.CriminalDamage$residuals-1))   *summary(Fit.OneOut.CriminalDamage)$coef[2,2])   -100
Summary.Table.OneOut$UL[6]  <- 100*exp(Fit.OneOut.MotorVehicleTheft$coefficients[2] + qt(.975, length(Fit.OneOut.MotorVehicleTheft$residuals-1))*summary(Fit.OneOut.MotorVehicleTheft)$coef[2,2])-100
Summary.Table.OneOut$UL[7]  <- 100*exp(Fit.OneOut.Robbery$coefficients[2]           + qt(.975, length(Fit.OneOut.Robbery$residuals-1))          *summary(Fit.OneOut.Robbery)$coef[2,2])          -100
Summary.Table.OneOut$UL[8]  <- 100*exp(Fit.OneOut.Assault$coefficients[2]           + qt(.975, length(Fit.OneOut.Assault$residuals-1))          *summary(Fit.OneOut.Assault)$coef[2,2])          -100
Summary.Table.OneOut$UL[9]  <- 100*exp(Fit.OneOut.Burglary$coefficients[2]          + qt(.975, length(Fit.OneOut.Burglary$residuals-1))         *summary(Fit.OneOut.Burglary)$coef[2,2])         -100
Summary.Table.OneOut$UL[10] <- 100*exp(Fit.OneOut.Homicide$coefficients[2]          + qt(.975, length(Fit.OneOut.Homicide$residuals-1))         *summary(Fit.OneOut.Homicide)$coef[2,2])         -100
Summary.Table.OneOut$UL[11] <- 100*exp(Fit.OneOut.DeceptivePractice$coefficients[2] + qt(.975, length(Fit.OneOut.DeceptivePractice$residuals-1))*summary(Fit.OneOut.DeceptivePractice)$coef[2,2])-100
Summary.Table.OneOut$star[which(Summary.Table.OneOut$p.value<0.01)]                                     <- rep("**", length(which(Summary.Table.OneOut$p.value<0.01)))
Summary.Table.OneOut$star[which(Summary.Table.OneOut$p.value<0.05 & Summary.Table.OneOut$p.value>=0.01)] <- rep("*" , length(which(Summary.Table.OneOut$p.value<0.05 & Summary.Table.OneOut$p.value>=0.01)))




Street.Lights.AllOut.Pois <- Street.Lights.AllOut[rep(seq(nrow(Street.Lights.AllOut)),3),]
Street.Lights.AllOut.Pois <- Street.Lights.AllOut.Pois[order(Street.Lights.AllOut.Pois$Service.Request.No),]
Street.Lights.AllOut.Pois$Time              <- rep(c("BEFORE", "DURING", "AFTER"), nrow(Street.Lights.AllOut))
Street.Lights.AllOut.Pois$AllCrimes         <- numeric(nrow(Street.Lights.AllOut.Pois))
Street.Lights.AllOut.Pois$Theft            <- numeric(nrow(Street.Lights.AllOut.Pois))
Street.Lights.AllOut.Pois$Narcotics         <- numeric(nrow(Street.Lights.AllOut.Pois))
Street.Lights.AllOut.Pois$Battery           <- numeric(nrow(Street.Lights.AllOut.Pois))
Street.Lights.AllOut.Pois$CriminalDamage    <- numeric(nrow(Street.Lights.AllOut.Pois))
Street.Lights.AllOut.Pois$MotorVehicleTheft <- numeric(nrow(Street.Lights.AllOut.Pois))
Street.Lights.AllOut.Pois$Robbery           <- numeric(nrow(Street.Lights.AllOut.Pois))
Street.Lights.AllOut.Pois$Assault           <- numeric(nrow(Street.Lights.AllOut.Pois))
Street.Lights.AllOut.Pois$Burglary          <- numeric(nrow(Street.Lights.AllOut.Pois))
Street.Lights.AllOut.Pois$Homicide          <- numeric(nrow(Street.Lights.AllOut.Pois))
Street.Lights.AllOut.Pois$DeceptivePractice <- numeric(nrow(Street.Lights.AllOut.Pois))
Street.Lights.AllOut.Pois$Duration          <- numeric(nrow(Street.Lights.AllOut.Pois))
Street.Lights.AllOut.Pois$AllCrimes[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]         <- Street.Lights.AllOut.Pois$Crimes.All.Before[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]
Street.Lights.AllOut.Pois$Theft[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]            <- Street.Lights.AllOut.Pois$Theft.Before[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]
Street.Lights.AllOut.Pois$Narcotics[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]         <- Street.Lights.AllOut.Pois$Narcotics.Before[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]
Street.Lights.AllOut.Pois$Battery[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]           <- Street.Lights.AllOut.Pois$Battery.Before[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]
Street.Lights.AllOut.Pois$CriminalDamage[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]    <- Street.Lights.AllOut.Pois$CriminalDamage.Before[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]
Street.Lights.AllOut.Pois$MotorVehicleTheft[which(Street.Lights.AllOut.Pois$Time=="BEFORE")] <- Street.Lights.AllOut.Pois$MotorVehicleTheft.Before[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]
Street.Lights.AllOut.Pois$Robbery[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]           <- Street.Lights.AllOut.Pois$Robbery.Before[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]
Street.Lights.AllOut.Pois$Assault[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]           <- Street.Lights.AllOut.Pois$Assault.Before[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]
Street.Lights.AllOut.Pois$Burglary[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]          <- Street.Lights.AllOut.Pois$Burglary.Before[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]
Street.Lights.AllOut.Pois$Homicide[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]          <- Street.Lights.AllOut.Pois$Homicide.Before[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]
Street.Lights.AllOut.Pois$DeceptivePractice[which(Street.Lights.AllOut.Pois$Time=="BEFORE")] <- Street.Lights.AllOut.Pois$DeceptivePractice.Before[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]
Street.Lights.AllOut.Pois$Duration[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]          <- rep(30, length(which(Street.Lights.AllOut.Pois$Time=="BEFORE")))

Street.Lights.AllOut.Pois$AllCrimes[which(Street.Lights.AllOut.Pois$Time=="DURING")]         <- Street.Lights.AllOut.Pois$Crimes.All.During[which(Street.Lights.AllOut.Pois$Time=="DURING")]
Street.Lights.AllOut.Pois$Theft[which(Street.Lights.AllOut.Pois$Time=="DURING")]            <- Street.Lights.AllOut.Pois$Theft.During[which(Street.Lights.AllOut.Pois$Time=="DURING")]
Street.Lights.AllOut.Pois$Narcotics[which(Street.Lights.AllOut.Pois$Time=="DURING")]         <- Street.Lights.AllOut.Pois$Narcotics.During[which(Street.Lights.AllOut.Pois$Time=="DURING")]
Street.Lights.AllOut.Pois$Battery[which(Street.Lights.AllOut.Pois$Time=="DURING")]           <- Street.Lights.AllOut.Pois$Battery.During[which(Street.Lights.AllOut.Pois$Time=="DURING")]
Street.Lights.AllOut.Pois$CriminalDamage[which(Street.Lights.AllOut.Pois$Time=="DURING")]    <- Street.Lights.AllOut.Pois$CriminalDamage.During[which(Street.Lights.AllOut.Pois$Time=="DURING")]
Street.Lights.AllOut.Pois$MotorVehicleTheft[which(Street.Lights.AllOut.Pois$Time=="DURING")] <- Street.Lights.AllOut.Pois$MotorVehicleTheft.During[which(Street.Lights.AllOut.Pois$Time=="DURING")]
Street.Lights.AllOut.Pois$Robbery[which(Street.Lights.AllOut.Pois$Time=="DURING")]           <- Street.Lights.AllOut.Pois$Robbery.During[which(Street.Lights.AllOut.Pois$Time=="DURING")]
Street.Lights.AllOut.Pois$Assault[which(Street.Lights.AllOut.Pois$Time=="DURING")]           <- Street.Lights.AllOut.Pois$Assault.During[which(Street.Lights.AllOut.Pois$Time=="DURING")]
Street.Lights.AllOut.Pois$Burglary[which(Street.Lights.AllOut.Pois$Time=="DURING")]          <- Street.Lights.AllOut.Pois$Burglary.During[which(Street.Lights.AllOut.Pois$Time=="DURING")]
Street.Lights.AllOut.Pois$Homicide[which(Street.Lights.AllOut.Pois$Time=="DURING")]          <- Street.Lights.AllOut.Pois$Homicide.During[which(Street.Lights.AllOut.Pois$Time=="DURING")]
Street.Lights.AllOut.Pois$DeceptivePractice[which(Street.Lights.AllOut.Pois$Time=="DURING")] <- Street.Lights.AllOut.Pois$DeceptivePractice.During[which(Street.Lights.AllOut.Pois$Time=="DURING")]
Street.Lights.AllOut.Pois$Duration[which(Street.Lights.AllOut.Pois$Time=="DURING")]          <- Street.Lights.AllOut.Pois$OutageDuration[which(Street.Lights.AllOut.Pois$Time=="DURING")]

Street.Lights.AllOut.Pois$AllCrimes[which(Street.Lights.AllOut.Pois$Time=="AFTER")]         <- Street.Lights.AllOut.Pois$Crimes.All.After[which(Street.Lights.AllOut.Pois$Time=="AFTER")]
Street.Lights.AllOut.Pois$Theft[which(Street.Lights.AllOut.Pois$Time=="AFTER")]            <- Street.Lights.AllOut.Pois$Theft.After[which(Street.Lights.AllOut.Pois$Time=="AFTER")]
Street.Lights.AllOut.Pois$Narcotics[which(Street.Lights.AllOut.Pois$Time=="AFTER")]         <- Street.Lights.AllOut.Pois$Narcotics.After[which(Street.Lights.AllOut.Pois$Time=="AFTER")]
Street.Lights.AllOut.Pois$Battery[which(Street.Lights.AllOut.Pois$Time=="AFTER")]           <- Street.Lights.AllOut.Pois$Battery.After[which(Street.Lights.AllOut.Pois$Time=="AFTER")]
Street.Lights.AllOut.Pois$CriminalDamage[which(Street.Lights.AllOut.Pois$Time=="AFTER")]    <- Street.Lights.AllOut.Pois$CriminalDamage.After[which(Street.Lights.AllOut.Pois$Time=="AFTER")]
Street.Lights.AllOut.Pois$MotorVehicleTheft[which(Street.Lights.AllOut.Pois$Time=="AFTER")] <- Street.Lights.AllOut.Pois$MotorVehicleTheft.After[which(Street.Lights.AllOut.Pois$Time=="AFTER")]
Street.Lights.AllOut.Pois$Robbery[which(Street.Lights.AllOut.Pois$Time=="AFTER")]           <- Street.Lights.AllOut.Pois$Robbery.After[which(Street.Lights.AllOut.Pois$Time=="AFTER")]
Street.Lights.AllOut.Pois$Assault[which(Street.Lights.AllOut.Pois$Time=="AFTER")]           <- Street.Lights.AllOut.Pois$Assault.After[which(Street.Lights.AllOut.Pois$Time=="AFTER")]
Street.Lights.AllOut.Pois$Burglary[which(Street.Lights.AllOut.Pois$Time=="AFTER")]          <- Street.Lights.AllOut.Pois$Burglary.After[which(Street.Lights.AllOut.Pois$Time=="AFTER")]
Street.Lights.AllOut.Pois$Homicide[which(Street.Lights.AllOut.Pois$Time=="AFTER")]          <- Street.Lights.AllOut.Pois$Homicide.After[which(Street.Lights.AllOut.Pois$Time=="AFTER")]
Street.Lights.AllOut.Pois$DeceptivePractice[which(Street.Lights.AllOut.Pois$Time=="AFTER")] <- Street.Lights.AllOut.Pois$DeceptivePractice.After[which(Street.Lights.AllOut.Pois$Time=="AFTER")]
Street.Lights.AllOut.Pois$Duration[which(Street.Lights.AllOut.Pois$Time=="AFTER")]          <- Street.Lights.AllOut.Pois$After.Period.Duration[which(Street.Lights.AllOut.Pois$Time=="AFTER")]

Street.Lights.AllOut.Pois <- Street.Lights.AllOut.Pois[,c("Type", "Service.Request.No","DateCreated", "DateCompleted", "Location", "Outcome", "x_coord", "y_coord", "zip_code", "ward",
                                                          "police_district", "community_area", "Time", "AllCrimes", "Theft", "Narcotics", "Battery", "CriminalDamage", 
                                                          "MotorVehicleTheft", "Robbery", "Assault", "Burglary", "Homicide", "DeceptivePractice", "Duration")]
Street.Lights.AllOut.Pois$OutageInd <- numeric(nrow(Street.Lights.AllOut.Pois))
Street.Lights.AllOut.Pois$OutageInd[which(Street.Lights.AllOut.Pois$Time=="DURING")] <- rep(1, length(which(Street.Lights.AllOut.Pois$Time=="DURING")))

Fit.AllOut.AllCrimes         <- glm(AllCrimes         ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Theft             <- glm(Theft             ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Narcotics         <- glm(Narcotics         ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Battery           <- glm(Battery           ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.CriminalDamage    <- glm(CriminalDamage    ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.MotorVehicleTheft <- glm(MotorVehicleTheft ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Robbery           <- glm(Robbery           ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Assault           <- glm(Assault           ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Burglary          <- glm(Burglary          ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Homicide          <- glm(Homicide          ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.DeceptivePractice <- glm(DeceptivePractice ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Summary.Table.AllOut$LL <- numeric(nrow(Summary.Table.AllOut))
Summary.Table.AllOut$UL <- numeric(nrow(Summary.Table.AllOut))
Summary.Table.AllOut$p.value <- numeric(nrow(Summary.Table.AllOut))
Summary.Table.AllOut$star <- character(nrow(Summary.Table.AllOut))
Summary.Table.AllOut$p.value[1] <- summary(Fit.AllOut.AllCrimes)$coef[2,4]
Summary.Table.AllOut$p.value[2] <- summary(Fit.AllOut.Theft)$coef[2,4]
Summary.Table.AllOut$p.value[3] <- summary(Fit.AllOut.Narcotics)$coef[2,4]
Summary.Table.AllOut$p.value[4] <- summary(Fit.AllOut.Battery)$coef[2,4]
Summary.Table.AllOut$p.value[5] <- summary(Fit.AllOut.CriminalDamage)$coef[2,4]
Summary.Table.AllOut$p.value[6] <- summary(Fit.AllOut.MotorVehicleTheft)$coef[2,4]
Summary.Table.AllOut$p.value[7] <- summary(Fit.AllOut.Robbery)$coef[2,4]
Summary.Table.AllOut$p.value[8] <- summary(Fit.AllOut.Assault)$coef[2,4]
Summary.Table.AllOut$p.value[9] <- summary(Fit.AllOut.Burglary)$coef[2,4]
Summary.Table.AllOut$p.value[10] <- summary(Fit.AllOut.Homicide)$coef[2,4]
Summary.Table.AllOut$p.value[11] <- summary(Fit.AllOut.DeceptivePractice)$coef[2,4]
Summary.Table.AllOut$LL[1]  <- 100*exp(Fit.AllOut.AllCrimes$coefficients[2]         - qt(.975, length(Fit.AllOut.AllCrimes$residuals-1))        *summary(Fit.AllOut.AllCrimes)$coef[2,2])        -100
Summary.Table.AllOut$LL[2]  <- 100*exp(Fit.AllOut.Theft$coefficients[2]            - qt(.975, length(Fit.AllOut.Theft$residuals-1))           *summary(Fit.AllOut.Theft)$coef[2,2])           -100
Summary.Table.AllOut$LL[3]  <- 100*exp(Fit.AllOut.Narcotics$coefficients[2]         - qt(.975, length(Fit.AllOut.Narcotics$residuals-1))        *summary(Fit.AllOut.Narcotics)$coef[2,2])        -100
Summary.Table.AllOut$LL[4]  <- 100*exp(Fit.AllOut.Battery$coefficients[2]           - qt(.975, length(Fit.AllOut.Battery$residuals-1))          *summary(Fit.AllOut.Battery)$coef[2,2])          -100
Summary.Table.AllOut$LL[5]  <- 100*exp(Fit.AllOut.CriminalDamage$coefficients[2]    - qt(.975, length(Fit.AllOut.CriminalDamage$residuals-1))   *summary(Fit.AllOut.CriminalDamage)$coef[2,2])   -100
Summary.Table.AllOut$LL[6]  <- 100*exp(Fit.AllOut.MotorVehicleTheft$coefficients[2] - qt(.975, length(Fit.AllOut.MotorVehicleTheft$residuals-1))*summary(Fit.AllOut.MotorVehicleTheft)$coef[2,2])-100
Summary.Table.AllOut$LL[7]  <- 100*exp(Fit.AllOut.Robbery$coefficients[2]           - qt(.975, length(Fit.AllOut.Robbery$residuals-1))          *summary(Fit.AllOut.Robbery)$coef[2,2])          -100
Summary.Table.AllOut$LL[8]  <- 100*exp(Fit.AllOut.Assault$coefficients[2]           - qt(.975, length(Fit.AllOut.Assault$residuals-1))          *summary(Fit.AllOut.Assault)$coef[2,2])          -100
Summary.Table.AllOut$LL[9]  <- 100*exp(Fit.AllOut.Burglary$coefficients[2]          - qt(.975, length(Fit.AllOut.Burglary$residuals-1))         *summary(Fit.AllOut.Burglary)$coef[2,2])         -100
Summary.Table.AllOut$LL[10] <- 100*exp(Fit.AllOut.Homicide$coefficients[2]          - qt(.975, length(Fit.AllOut.Homicide$residuals-1))         *summary(Fit.AllOut.Homicide)$coef[2,2])         -100
Summary.Table.AllOut$LL[11] <- 100*exp(Fit.AllOut.DeceptivePractice$coefficients[2] - qt(.975, length(Fit.AllOut.DeceptivePractice$residuals-1))*summary(Fit.AllOut.DeceptivePractice)$coef[2,2])-100
Summary.Table.AllOut$UL[1]  <- 100*exp(Fit.AllOut.AllCrimes$coefficients[2]         + qt(.975, length(Fit.AllOut.AllCrimes$residuals-1))        *summary(Fit.AllOut.AllCrimes)$coef[2,2])        -100
Summary.Table.AllOut$UL[2]  <- 100*exp(Fit.AllOut.Theft$coefficients[2]             + qt(.975, length(Fit.AllOut.Theft$residuals-1))           *summary(Fit.AllOut.Theft)$coef[2,2])           -100
Summary.Table.AllOut$UL[3]  <- 100*exp(Fit.AllOut.Narcotics$coefficients[2]         + qt(.975, length(Fit.AllOut.Narcotics$residuals-1))        *summary(Fit.AllOut.Narcotics)$coef[2,2])        -100
Summary.Table.AllOut$UL[4]  <- 100*exp(Fit.AllOut.Battery$coefficients[2]           + qt(.975, length(Fit.AllOut.Battery$residuals-1))          *summary(Fit.AllOut.Battery)$coef[2,2])          -100
Summary.Table.AllOut$UL[5]  <- 100*exp(Fit.AllOut.CriminalDamage$coefficients[2]    + qt(.975, length(Fit.AllOut.CriminalDamage$residuals-1))   *summary(Fit.AllOut.CriminalDamage)$coef[2,2])   -100
Summary.Table.AllOut$UL[6]  <- 100*exp(Fit.AllOut.MotorVehicleTheft$coefficients[2] + qt(.975, length(Fit.AllOut.MotorVehicleTheft$residuals-1))*summary(Fit.AllOut.MotorVehicleTheft)$coef[2,2])-100
Summary.Table.AllOut$UL[7]  <- 100*exp(Fit.AllOut.Robbery$coefficients[2]           + qt(.975, length(Fit.AllOut.Robbery$residuals-1))          *summary(Fit.AllOut.Robbery)$coef[2,2])          -100
Summary.Table.AllOut$UL[8]  <- 100*exp(Fit.AllOut.Assault$coefficients[2]           + qt(.975, length(Fit.AllOut.Assault$residuals-1))          *summary(Fit.AllOut.Assault)$coef[2,2])          -100
Summary.Table.AllOut$UL[9]  <- 100*exp(Fit.AllOut.Burglary$coefficients[2]          + qt(.975, length(Fit.AllOut.Burglary$residuals-1))         *summary(Fit.AllOut.Burglary)$coef[2,2])         -100
Summary.Table.AllOut$UL[10] <- 100*exp(Fit.AllOut.Homicide$coefficients[2]          + qt(.975, length(Fit.AllOut.Homicide$residuals-1))         *summary(Fit.AllOut.Homicide)$coef[2,2])         -100
Summary.Table.AllOut$UL[11] <- 100*exp(Fit.AllOut.DeceptivePractice$coefficients[2] + qt(.975, length(Fit.AllOut.DeceptivePractice$residuals-1))*summary(Fit.AllOut.DeceptivePractice)$coef[2,2])-100
Summary.Table.AllOut$star[which(Summary.Table.AllOut$p.value<0.01)]                                     <- rep("**", length(which(Summary.Table.AllOut$p.value<0.01)))
Summary.Table.AllOut$star[which(Summary.Table.AllOut$p.value<0.05 & Summary.Table.AllOut$p.value>=0.01)] <- rep("*" , length(which(Summary.Table.AllOut$p.value<0.05 & Summary.Table.AllOut$p.value>=0.01)))





## Seasonality Specifications - Based on Midpoint of Period
Alley.Lights.Pois$Month <- numeric(nrow(Alley.Lights.Pois))
Alley.Lights.Pois$Month[which(Alley.Lights.Pois$Time=="BEFORE")] <- as.numeric(substr(as.Date(Alley.Lights.Pois$DateCreated, "%m/%d/%Y")-22,6,7))[which(Alley.Lights.Pois$Time=="BEFORE")] 
Alley.Lights.Pois$Month[which(Alley.Lights.Pois$Time=="DURING")] <- as.numeric(substr(as.Date(Alley.Lights.Pois$DateCreated, "%m/%d/%Y")+round(Alley.Lights.Pois$Duration/2,0),6,7))[which(Alley.Lights.Pois$Time=="DURING")] 
Alley.Lights.Pois$Month[which(Alley.Lights.Pois$Time=="AFTER")]  <- as.numeric(substr(as.Date(Alley.Lights.Pois$DateCompleted, "%m/%d/%Y")+7+round(Alley.Lights.Pois$Duration/2,0),6,7))[which(Alley.Lights.Pois$Time=="AFTER")] 
Alley.Lights.Pois$Year <- numeric(nrow(Alley.Lights.Pois))
Alley.Lights.Pois$Year[which(Alley.Lights.Pois$Time=="BEFORE")] <- as.numeric(substr(as.Date(Alley.Lights.Pois$DateCreated, "%m/%d/%Y")-22,3,4))[which(Alley.Lights.Pois$Time=="BEFORE")]  
Alley.Lights.Pois$Year[which(Alley.Lights.Pois$Time=="DURING")] <- as.numeric(substr(as.Date(Alley.Lights.Pois$DateCreated, "%m/%d/%Y")+round(Alley.Lights.Pois$Duration/2,0),3,4))[which(Alley.Lights.Pois$Time=="DURING")] 
Alley.Lights.Pois$Year[which(Alley.Lights.Pois$Time=="AFTER")]  <- as.numeric(substr(as.Date(Alley.Lights.Pois$DateCompleted, "%m/%d/%Y")+7+round(Alley.Lights.Pois$Duration/2,0),3,4))[which(Alley.Lights.Pois$Time=="AFTER")] 

Alley.Lights.Pois$Mar12 <- as.numeric(Alley.Lights.Pois$Month==3)  * as.numeric(Alley.Lights.Pois$Year==12)
Alley.Lights.Pois$Apr12 <- as.numeric(Alley.Lights.Pois$Month==4)  * as.numeric(Alley.Lights.Pois$Year==12)
Alley.Lights.Pois$May12 <- as.numeric(Alley.Lights.Pois$Month==5)  * as.numeric(Alley.Lights.Pois$Year==12)
Alley.Lights.Pois$Jun12 <- as.numeric(Alley.Lights.Pois$Month==6)  * as.numeric(Alley.Lights.Pois$Year==12)
Alley.Lights.Pois$Jul12 <- as.numeric(Alley.Lights.Pois$Month==7)  * as.numeric(Alley.Lights.Pois$Year==12) 
Alley.Lights.Pois$Aug12 <- as.numeric(Alley.Lights.Pois$Month==8)  * as.numeric(Alley.Lights.Pois$Year==12)
Alley.Lights.Pois$Sep12 <- as.numeric(Alley.Lights.Pois$Month==9)  * as.numeric(Alley.Lights.Pois$Year==12)
Alley.Lights.Pois$Oct12 <- as.numeric(Alley.Lights.Pois$Month==10) * as.numeric(Alley.Lights.Pois$Year==12)
Alley.Lights.Pois$Nov12 <- as.numeric(Alley.Lights.Pois$Month==11) * as.numeric(Alley.Lights.Pois$Year==12)
Alley.Lights.Pois$Dec12 <- as.numeric(Alley.Lights.Pois$Month==12) * as.numeric(Alley.Lights.Pois$Year==12)
Alley.Lights.Pois$Jan13 <- as.numeric(Alley.Lights.Pois$Month==1)  * as.numeric(Alley.Lights.Pois$Year==13)
Alley.Lights.Pois$Feb13 <- as.numeric(Alley.Lights.Pois$Month==2)  * as.numeric(Alley.Lights.Pois$Year==13)
Alley.Lights.Pois$Mar13 <- as.numeric(Alley.Lights.Pois$Month==3)  * as.numeric(Alley.Lights.Pois$Year==13)
Alley.Lights.Pois$Apr13 <- as.numeric(Alley.Lights.Pois$Month==4)  * as.numeric(Alley.Lights.Pois$Year==13)
Alley.Lights.Pois$May13 <- as.numeric(Alley.Lights.Pois$Month==5)  * as.numeric(Alley.Lights.Pois$Year==13)
Alley.Lights.Pois$Jun13 <- as.numeric(Alley.Lights.Pois$Month==6)  * as.numeric(Alley.Lights.Pois$Year==13)
Alley.Lights.Pois$Jul13 <- as.numeric(Alley.Lights.Pois$Month==7)  * as.numeric(Alley.Lights.Pois$Year==13) 

Fit.Alley.AllCrimes.Seas         <- glm(AllCrimes         ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Theft.Seas            <- glm(Theft            ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Narcotics.Seas         <- glm(Narcotics         ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Battery.Seas           <- glm(Battery           ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.CriminalDamage.Seas    <- glm(CriminalDamage    ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.MotorVehicleTheft.Seas <- glm(MotorVehicleTheft ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Robbery.Seas           <- glm(Robbery           ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Assault.Seas           <- glm(Assault           ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Burglary.Seas          <- glm(Burglary          ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Homicide.Seas          <- glm(Homicide          ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.DeceptivePractice.Seas <- glm(DeceptivePractice ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Alley.Lights.Pois)
Summary.Table.Alley$PctDiff.Seas <- numeric(nrow(Summary.Table.Alley))
Summary.Table.Alley$LL.Seas <- numeric(nrow(Summary.Table.Alley))
Summary.Table.Alley$UL.Seas <- numeric(nrow(Summary.Table.Alley))
Summary.Table.Alley$p.value.Seas <- numeric(nrow(Summary.Table.Alley))
Summary.Table.Alley$star.Seas <- character(nrow(Summary.Table.Alley))
Summary.Table.Alley$PctDiff.Seas[1] <- 100*exp(Fit.Alley.AllCrimes.Seas$coef[2])-100
Summary.Table.Alley$PctDiff.Seas[2] <- 100*exp(Fit.Alley.Theft.Seas$coef[2])-100
Summary.Table.Alley$PctDiff.Seas[3] <- 100*exp(Fit.Alley.Narcotics.Seas$coef[2])-100
Summary.Table.Alley$PctDiff.Seas[4] <- 100*exp(Fit.Alley.Battery.Seas$coef[2])-100
Summary.Table.Alley$PctDiff.Seas[5] <- 100*exp(Fit.Alley.CriminalDamage.Seas$coef[2])-100
Summary.Table.Alley$PctDiff.Seas[6] <- 100*exp(Fit.Alley.MotorVehicleTheft.Seas$coef[2])-100
Summary.Table.Alley$PctDiff.Seas[7] <- 100*exp(Fit.Alley.Robbery.Seas$coef[2])-100
Summary.Table.Alley$PctDiff.Seas[8] <- 100*exp(Fit.Alley.Assault.Seas$coef[2])-100
Summary.Table.Alley$PctDiff.Seas[9] <- 100*exp(Fit.Alley.Burglary.Seas$coef[2])-100
Summary.Table.Alley$PctDiff.Seas[10] <- 100*exp(Fit.Alley.Homicide.Seas$coef[2])-100
Summary.Table.Alley$PctDiff.Seas[11] <- 100*exp(Fit.Alley.DeceptivePractice.Seas$coef[2])-100
Summary.Table.Alley$p.value.Seas[1] <- summary(Fit.Alley.AllCrimes.Seas)$coef[2,4]
Summary.Table.Alley$p.value.Seas[2] <- summary(Fit.Alley.Theft.Seas)$coef[2,4]
Summary.Table.Alley$p.value.Seas[3] <- summary(Fit.Alley.Narcotics.Seas)$coef[2,4]
Summary.Table.Alley$p.value.Seas[4] <- summary(Fit.Alley.Battery.Seas)$coef[2,4]
Summary.Table.Alley$p.value.Seas[5] <- summary(Fit.Alley.CriminalDamage.Seas)$coef[2,4]
Summary.Table.Alley$p.value.Seas[6] <- summary(Fit.Alley.MotorVehicleTheft.Seas)$coef[2,4]
Summary.Table.Alley$p.value.Seas[7] <- summary(Fit.Alley.Robbery.Seas)$coef[2,4]
Summary.Table.Alley$p.value.Seas[8] <- summary(Fit.Alley.Assault.Seas)$coef[2,4]
Summary.Table.Alley$p.value.Seas[9] <- summary(Fit.Alley.Burglary.Seas)$coef[2,4]
Summary.Table.Alley$p.value.Seas[10] <- summary(Fit.Alley.Homicide.Seas)$coef[2,4]
Summary.Table.Alley$p.value.Seas[11] <- summary(Fit.Alley.DeceptivePractice.Seas)$coef[2,4]
Summary.Table.Alley$LL.Seas[1]  <- 100*exp(Fit.Alley.AllCrimes.Seas$coefficients[2]         - qt(.975, length(Fit.Alley.AllCrimes.Seas$residuals-1))        *summary(Fit.Alley.AllCrimes.Seas)$coef[2,2])        -100
Summary.Table.Alley$LL.Seas[2]  <- 100*exp(Fit.Alley.Theft.Seas$coefficients[2]             - qt(.975, length(Fit.Alley.Theft.Seas$residuals-1))           *summary(Fit.Alley.Theft.Seas)$coef[2,2])           -100
Summary.Table.Alley$LL.Seas[3]  <- 100*exp(Fit.Alley.Narcotics.Seas$coefficients[2]         - qt(.975, length(Fit.Alley.Narcotics.Seas$residuals-1))        *summary(Fit.Alley.Narcotics.Seas)$coef[2,2])        -100
Summary.Table.Alley$LL.Seas[4]  <- 100*exp(Fit.Alley.Battery.Seas$coefficients[2]           - qt(.975, length(Fit.Alley.Battery.Seas$residuals-1))          *summary(Fit.Alley.Battery.Seas)$coef[2,2])          -100
Summary.Table.Alley$LL.Seas[5]  <- 100*exp(Fit.Alley.CriminalDamage.Seas$coefficients[2]    - qt(.975, length(Fit.Alley.CriminalDamage.Seas$residuals-1))   *summary(Fit.Alley.CriminalDamage.Seas)$coef[2,2])   -100
Summary.Table.Alley$LL.Seas[6]  <- 100*exp(Fit.Alley.MotorVehicleTheft.Seas$coefficients[2] - qt(.975, length(Fit.Alley.MotorVehicleTheft.Seas$residuals-1))*summary(Fit.Alley.MotorVehicleTheft.Seas)$coef[2,2])-100
Summary.Table.Alley$LL.Seas[7]  <- 100*exp(Fit.Alley.Robbery.Seas$coefficients[2]           - qt(.975, length(Fit.Alley.Robbery.Seas$residuals-1))          *summary(Fit.Alley.Robbery.Seas)$coef[2,2])          -100
Summary.Table.Alley$LL.Seas[8]  <- 100*exp(Fit.Alley.Assault.Seas$coefficients[2]           - qt(.975, length(Fit.Alley.Assault.Seas$residuals-1))          *summary(Fit.Alley.Assault.Seas)$coef[2,2])          -100
Summary.Table.Alley$LL.Seas[9]  <- 100*exp(Fit.Alley.Burglary.Seas$coefficients[2]          - qt(.975, length(Fit.Alley.Burglary.Seas$residuals-1))         *summary(Fit.Alley.Burglary.Seas)$coef[2,2])         -100
Summary.Table.Alley$LL.Seas[10] <- 100*exp(Fit.Alley.Homicide.Seas$coefficients[2]          - qt(.975, length(Fit.Alley.Homicide.Seas$residuals-1))         *summary(Fit.Alley.Homicide.Seas)$coef[2,2])         -100
Summary.Table.Alley$LL.Seas[11] <- 100*exp(Fit.Alley.DeceptivePractice.Seas$coefficients[2] - qt(.975, length(Fit.Alley.DeceptivePractice.Seas$residuals-1))*summary(Fit.Alley.DeceptivePractice.Seas)$coef[2,2])-100
Summary.Table.Alley$UL.Seas[1]  <- 100*exp(Fit.Alley.AllCrimes.Seas$coefficients[2]         + qt(.975, length(Fit.Alley.AllCrimes.Seas$residuals-1))        *summary(Fit.Alley.AllCrimes.Seas)$coef[2,2])        -100
Summary.Table.Alley$UL.Seas[2]  <- 100*exp(Fit.Alley.Theft.Seas$coefficients[2]             + qt(.975, length(Fit.Alley.Theft.Seas$residuals-1))           *summary(Fit.Alley.Theft.Seas)$coef[2,2])           -100
Summary.Table.Alley$UL.Seas[3]  <- 100*exp(Fit.Alley.Narcotics.Seas$coefficients[2]         + qt(.975, length(Fit.Alley.Narcotics.Seas$residuals-1))        *summary(Fit.Alley.Narcotics.Seas)$coef[2,2])        -100
Summary.Table.Alley$UL.Seas[4]  <- 100*exp(Fit.Alley.Battery.Seas$coefficients[2]           + qt(.975, length(Fit.Alley.Battery.Seas$residuals-1))          *summary(Fit.Alley.Battery.Seas)$coef[2,2])          -100
Summary.Table.Alley$UL.Seas[5]  <- 100*exp(Fit.Alley.CriminalDamage.Seas$coefficients[2]    + qt(.975, length(Fit.Alley.CriminalDamage.Seas$residuals-1))   *summary(Fit.Alley.CriminalDamage.Seas)$coef[2,2])   -100
Summary.Table.Alley$UL.Seas[6]  <- 100*exp(Fit.Alley.MotorVehicleTheft.Seas$coefficients[2] + qt(.975, length(Fit.Alley.MotorVehicleTheft.Seas$residuals-1))*summary(Fit.Alley.MotorVehicleTheft.Seas)$coef[2,2])-100
Summary.Table.Alley$UL.Seas[7]  <- 100*exp(Fit.Alley.Robbery.Seas$coefficients[2]           + qt(.975, length(Fit.Alley.Robbery.Seas$residuals-1))          *summary(Fit.Alley.Robbery.Seas)$coef[2,2])          -100
Summary.Table.Alley$UL.Seas[8]  <- 100*exp(Fit.Alley.Assault.Seas$coefficients[2]           + qt(.975, length(Fit.Alley.Assault.Seas$residuals-1))          *summary(Fit.Alley.Assault.Seas)$coef[2,2])          -100
Summary.Table.Alley$UL.Seas[9]  <- 100*exp(Fit.Alley.Burglary.Seas$coefficients[2]          + qt(.975, length(Fit.Alley.Burglary.Seas$residuals-1))         *summary(Fit.Alley.Burglary.Seas)$coef[2,2])         -100
Summary.Table.Alley$UL.Seas[10] <- 100*exp(Fit.Alley.Homicide.Seas$coefficients[2]          + qt(.975, length(Fit.Alley.Homicide.Seas$residuals-1))         *summary(Fit.Alley.Homicide.Seas)$coef[2,2])         -100
Summary.Table.Alley$UL.Seas[11] <- 100*exp(Fit.Alley.DeceptivePractice.Seas$coefficients[2] + qt(.975, length(Fit.Alley.DeceptivePractice.Seas$residuals-1))*summary(Fit.Alley.DeceptivePractice.Seas)$coef[2,2])-100
Summary.Table.Alley$star.Seas[which(Summary.Table.Alley$p.value.Seas<0.01)]                                           <- rep("**", length(which(Summary.Table.Alley$p.value.Seas<0.01)))
Summary.Table.Alley$star.Seas[which(Summary.Table.Alley$p.value.Seas<0.05 & Summary.Table.Alley$p.value.Seas>=0.01)] <- rep("*" , length(which(Summary.Table.Alley$p.value.Seas<0.05 & Summary.Table.Alley$p.value.Seas>=0.01)))



Street.Lights.OneOut.Pois$Month <- numeric(nrow(Street.Lights.OneOut.Pois))
Street.Lights.OneOut.Pois$Month[which(Street.Lights.OneOut.Pois$Time=="BEFORE")] <- as.numeric(substr(as.Date(Street.Lights.OneOut.Pois$DateCreated, "%m/%d/%Y")-22,6,7))[which(Street.Lights.OneOut.Pois$Time=="BEFORE")] 
Street.Lights.OneOut.Pois$Month[which(Street.Lights.OneOut.Pois$Time=="DURING")] <- as.numeric(substr(as.Date(Street.Lights.OneOut.Pois$DateCreated, "%m/%d/%Y")+round(Street.Lights.OneOut.Pois$Duration/2,0),6,7))[which(Street.Lights.OneOut.Pois$Time=="DURING")] 
Street.Lights.OneOut.Pois$Month[which(Street.Lights.OneOut.Pois$Time=="AFTER")]  <- as.numeric(substr(as.Date(Street.Lights.OneOut.Pois$DateCompleted, "%m/%d/%Y")+7+round(Street.Lights.OneOut.Pois$Duration/2,0),6,7))[which(Street.Lights.OneOut.Pois$Time=="AFTER")] 
Street.Lights.OneOut.Pois$Year <- numeric(nrow(Street.Lights.OneOut.Pois))
Street.Lights.OneOut.Pois$Year[which(Street.Lights.OneOut.Pois$Time=="BEFORE")] <- as.numeric(substr(as.Date(Street.Lights.OneOut.Pois$DateCreated, "%m/%d/%Y")-22,3,4))[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]  
Street.Lights.OneOut.Pois$Year[which(Street.Lights.OneOut.Pois$Time=="DURING")] <- as.numeric(substr(as.Date(Street.Lights.OneOut.Pois$DateCreated, "%m/%d/%Y")+round(Street.Lights.OneOut.Pois$Duration/2,0),3,4))[which(Street.Lights.OneOut.Pois$Time=="DURING")] 
Street.Lights.OneOut.Pois$Year[which(Street.Lights.OneOut.Pois$Time=="AFTER")]  <- as.numeric(substr(as.Date(Street.Lights.OneOut.Pois$DateCompleted, "%m/%d/%Y")+7+round(Street.Lights.OneOut.Pois$Duration/2,0),3,4))[which(Street.Lights.OneOut.Pois$Time=="AFTER")] 

Street.Lights.OneOut.Pois$Mar12 <- as.numeric(Street.Lights.OneOut.Pois$Month==3)  * as.numeric(Street.Lights.OneOut.Pois$Year==12)
Street.Lights.OneOut.Pois$Apr12 <- as.numeric(Street.Lights.OneOut.Pois$Month==4)  * as.numeric(Street.Lights.OneOut.Pois$Year==12)
Street.Lights.OneOut.Pois$May12 <- as.numeric(Street.Lights.OneOut.Pois$Month==5)  * as.numeric(Street.Lights.OneOut.Pois$Year==12)
Street.Lights.OneOut.Pois$Jun12 <- as.numeric(Street.Lights.OneOut.Pois$Month==6)  * as.numeric(Street.Lights.OneOut.Pois$Year==12)
Street.Lights.OneOut.Pois$Jul12 <- as.numeric(Street.Lights.OneOut.Pois$Month==7)  * as.numeric(Street.Lights.OneOut.Pois$Year==12) 
Street.Lights.OneOut.Pois$Aug12 <- as.numeric(Street.Lights.OneOut.Pois$Month==8)  * as.numeric(Street.Lights.OneOut.Pois$Year==12)
Street.Lights.OneOut.Pois$Sep12 <- as.numeric(Street.Lights.OneOut.Pois$Month==9)  * as.numeric(Street.Lights.OneOut.Pois$Year==12)
Street.Lights.OneOut.Pois$Oct12 <- as.numeric(Street.Lights.OneOut.Pois$Month==10) * as.numeric(Street.Lights.OneOut.Pois$Year==12)
Street.Lights.OneOut.Pois$Nov12 <- as.numeric(Street.Lights.OneOut.Pois$Month==11) * as.numeric(Street.Lights.OneOut.Pois$Year==12)
Street.Lights.OneOut.Pois$Dec12 <- as.numeric(Street.Lights.OneOut.Pois$Month==12) * as.numeric(Street.Lights.OneOut.Pois$Year==12)
Street.Lights.OneOut.Pois$Jan13 <- as.numeric(Street.Lights.OneOut.Pois$Month==1)  * as.numeric(Street.Lights.OneOut.Pois$Year==13)
Street.Lights.OneOut.Pois$Feb13 <- as.numeric(Street.Lights.OneOut.Pois$Month==2)  * as.numeric(Street.Lights.OneOut.Pois$Year==13)
Street.Lights.OneOut.Pois$Mar13 <- as.numeric(Street.Lights.OneOut.Pois$Month==3)  * as.numeric(Street.Lights.OneOut.Pois$Year==13)
Street.Lights.OneOut.Pois$Apr13 <- as.numeric(Street.Lights.OneOut.Pois$Month==4)  * as.numeric(Street.Lights.OneOut.Pois$Year==13)
Street.Lights.OneOut.Pois$May13 <- as.numeric(Street.Lights.OneOut.Pois$Month==5)  * as.numeric(Street.Lights.OneOut.Pois$Year==13)
Street.Lights.OneOut.Pois$Jun13 <- as.numeric(Street.Lights.OneOut.Pois$Month==6)  * as.numeric(Street.Lights.OneOut.Pois$Year==13)
Street.Lights.OneOut.Pois$Jul13 <- as.numeric(Street.Lights.OneOut.Pois$Month==7)  * as.numeric(Street.Lights.OneOut.Pois$Year==13) 

Fit.OneOut.AllCrimes.Seas         <- glm(AllCrimes         ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Theft.Seas             <- glm(Theft             ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Narcotics.Seas         <- glm(Narcotics         ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Battery.Seas           <- glm(Battery           ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.CriminalDamage.Seas    <- glm(CriminalDamage    ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.MotorVehicleTheft.Seas <- glm(MotorVehicleTheft ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Robbery.Seas           <- glm(Robbery           ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Assault.Seas           <- glm(Assault           ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Burglary.Seas          <- glm(Burglary          ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Homicide.Seas          <- glm(Homicide          ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.DeceptivePractice.Seas <- glm(DeceptivePractice ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Summary.Table.OneOut$PctDiff.Seas <- numeric(nrow(Summary.Table.OneOut))
Summary.Table.OneOut$LL.Seas <- numeric(nrow(Summary.Table.OneOut))
Summary.Table.OneOut$UL.Seas <- numeric(nrow(Summary.Table.OneOut))
Summary.Table.OneOut$p.value.Seas <- numeric(nrow(Summary.Table.OneOut))
Summary.Table.OneOut$star.Seas <- character(nrow(Summary.Table.OneOut))
Summary.Table.OneOut$PctDiff.Seas[1] <- 100*exp(Fit.OneOut.AllCrimes.Seas$coef[2])-100
Summary.Table.OneOut$PctDiff.Seas[2] <- 100*exp(Fit.OneOut.Theft.Seas$coef[2])-100
Summary.Table.OneOut$PctDiff.Seas[3] <- 100*exp(Fit.OneOut.Narcotics.Seas$coef[2])-100
Summary.Table.OneOut$PctDiff.Seas[4] <- 100*exp(Fit.OneOut.Battery.Seas$coef[2])-100
Summary.Table.OneOut$PctDiff.Seas[5] <- 100*exp(Fit.OneOut.CriminalDamage.Seas$coef[2])-100
Summary.Table.OneOut$PctDiff.Seas[6] <- 100*exp(Fit.OneOut.MotorVehicleTheft.Seas$coef[2])-100
Summary.Table.OneOut$PctDiff.Seas[7] <- 100*exp(Fit.OneOut.Robbery.Seas$coef[2])-100
Summary.Table.OneOut$PctDiff.Seas[8] <- 100*exp(Fit.OneOut.Assault.Seas$coef[2])-100
Summary.Table.OneOut$PctDiff.Seas[9] <- 100*exp(Fit.OneOut.Burglary.Seas$coef[2])-100
Summary.Table.OneOut$PctDiff.Seas[10] <- 100*exp(Fit.OneOut.Homicide.Seas$coef[2])-100
Summary.Table.OneOut$PctDiff.Seas[11] <- 100*exp(Fit.OneOut.DeceptivePractice.Seas$coef[2])-100
Summary.Table.OneOut$p.value.Seas[1] <- summary(Fit.OneOut.AllCrimes.Seas)$coef[2,4]
Summary.Table.OneOut$p.value.Seas[2] <- summary(Fit.OneOut.Theft.Seas)$coef[2,4]
Summary.Table.OneOut$p.value.Seas[3] <- summary(Fit.OneOut.Narcotics.Seas)$coef[2,4]
Summary.Table.OneOut$p.value.Seas[4] <- summary(Fit.OneOut.Battery.Seas)$coef[2,4]
Summary.Table.OneOut$p.value.Seas[5] <- summary(Fit.OneOut.CriminalDamage.Seas)$coef[2,4]
Summary.Table.OneOut$p.value.Seas[6] <- summary(Fit.OneOut.MotorVehicleTheft.Seas)$coef[2,4]
Summary.Table.OneOut$p.value.Seas[7] <- summary(Fit.OneOut.Robbery.Seas)$coef[2,4]
Summary.Table.OneOut$p.value.Seas[8] <- summary(Fit.OneOut.Assault.Seas)$coef[2,4]
Summary.Table.OneOut$p.value.Seas[9] <- summary(Fit.OneOut.Burglary.Seas)$coef[2,4]
Summary.Table.OneOut$p.value.Seas[10] <- summary(Fit.OneOut.Homicide.Seas)$coef[2,4]
Summary.Table.OneOut$p.value.Seas[11] <- summary(Fit.OneOut.DeceptivePractice.Seas)$coef[2,4]
Summary.Table.OneOut$LL.Seas[1]  <- 100*exp(Fit.OneOut.AllCrimes.Seas$coefficients[2]         - qt(.975, length(Fit.OneOut.AllCrimes.Seas$residuals-1))        *summary(Fit.OneOut.AllCrimes.Seas)$coef[2,2])        -100
Summary.Table.OneOut$LL.Seas[2]  <- 100*exp(Fit.OneOut.Theft.Seas$coefficients[2]             - qt(.975, length(Fit.OneOut.Theft.Seas$residuals-1))           *summary(Fit.OneOut.Theft.Seas)$coef[2,2])           -100
Summary.Table.OneOut$LL.Seas[3]  <- 100*exp(Fit.OneOut.Narcotics.Seas$coefficients[2]         - qt(.975, length(Fit.OneOut.Narcotics.Seas$residuals-1))        *summary(Fit.OneOut.Narcotics.Seas)$coef[2,2])        -100
Summary.Table.OneOut$LL.Seas[4]  <- 100*exp(Fit.OneOut.Battery.Seas$coefficients[2]           - qt(.975, length(Fit.OneOut.Battery.Seas$residuals-1))          *summary(Fit.OneOut.Battery.Seas)$coef[2,2])          -100
Summary.Table.OneOut$LL.Seas[5]  <- 100*exp(Fit.OneOut.CriminalDamage.Seas$coefficients[2]    - qt(.975, length(Fit.OneOut.CriminalDamage.Seas$residuals-1))   *summary(Fit.OneOut.CriminalDamage.Seas)$coef[2,2])   -100
Summary.Table.OneOut$LL.Seas[6]  <- 100*exp(Fit.OneOut.MotorVehicleTheft.Seas$coefficients[2] - qt(.975, length(Fit.OneOut.MotorVehicleTheft.Seas$residuals-1))*summary(Fit.OneOut.MotorVehicleTheft.Seas)$coef[2,2])-100
Summary.Table.OneOut$LL.Seas[7]  <- 100*exp(Fit.OneOut.Robbery.Seas$coefficients[2]           - qt(.975, length(Fit.OneOut.Robbery.Seas$residuals-1))          *summary(Fit.OneOut.Robbery.Seas)$coef[2,2])          -100
Summary.Table.OneOut$LL.Seas[8]  <- 100*exp(Fit.OneOut.Assault.Seas$coefficients[2]           - qt(.975, length(Fit.OneOut.Assault.Seas$residuals-1))          *summary(Fit.OneOut.Assault.Seas)$coef[2,2])          -100
Summary.Table.OneOut$LL.Seas[9]  <- 100*exp(Fit.OneOut.Burglary.Seas$coefficients[2]          - qt(.975, length(Fit.OneOut.Burglary.Seas$residuals-1))         *summary(Fit.OneOut.Burglary.Seas)$coef[2,2])         -100
Summary.Table.OneOut$LL.Seas[10] <- 100*exp(Fit.OneOut.Homicide.Seas$coefficients[2]          - qt(.975, length(Fit.OneOut.Homicide.Seas$residuals-1))         *summary(Fit.OneOut.Homicide.Seas)$coef[2,2])         -100
Summary.Table.OneOut$LL.Seas[11] <- 100*exp(Fit.OneOut.DeceptivePractice.Seas$coefficients[2] - qt(.975, length(Fit.OneOut.DeceptivePractice.Seas$residuals-1))*summary(Fit.OneOut.DeceptivePractice.Seas)$coef[2,2])-100
Summary.Table.OneOut$UL.Seas[1]  <- 100*exp(Fit.OneOut.AllCrimes.Seas$coefficients[2]         + qt(.975, length(Fit.OneOut.AllCrimes.Seas$residuals-1))        *summary(Fit.OneOut.AllCrimes.Seas)$coef[2,2])        -100
Summary.Table.OneOut$UL.Seas[2]  <- 100*exp(Fit.OneOut.Theft.Seas$coefficients[2]             + qt(.975, length(Fit.OneOut.Theft.Seas$residuals-1))           *summary(Fit.OneOut.Theft.Seas)$coef[2,2])           -100
Summary.Table.OneOut$UL.Seas[3]  <- 100*exp(Fit.OneOut.Narcotics.Seas$coefficients[2]         + qt(.975, length(Fit.OneOut.Narcotics.Seas$residuals-1))        *summary(Fit.OneOut.Narcotics.Seas)$coef[2,2])        -100
Summary.Table.OneOut$UL.Seas[4]  <- 100*exp(Fit.OneOut.Battery.Seas$coefficients[2]           + qt(.975, length(Fit.OneOut.Battery.Seas$residuals-1))          *summary(Fit.OneOut.Battery.Seas)$coef[2,2])          -100
Summary.Table.OneOut$UL.Seas[5]  <- 100*exp(Fit.OneOut.CriminalDamage.Seas$coefficients[2]    + qt(.975, length(Fit.OneOut.CriminalDamage.Seas$residuals-1))   *summary(Fit.OneOut.CriminalDamage.Seas)$coef[2,2])   -100
Summary.Table.OneOut$UL.Seas[6]  <- 100*exp(Fit.OneOut.MotorVehicleTheft.Seas$coefficients[2] + qt(.975, length(Fit.OneOut.MotorVehicleTheft.Seas$residuals-1))*summary(Fit.OneOut.MotorVehicleTheft.Seas)$coef[2,2])-100
Summary.Table.OneOut$UL.Seas[7]  <- 100*exp(Fit.OneOut.Robbery.Seas$coefficients[2]           + qt(.975, length(Fit.OneOut.Robbery.Seas$residuals-1))          *summary(Fit.OneOut.Robbery.Seas)$coef[2,2])          -100
Summary.Table.OneOut$UL.Seas[8]  <- 100*exp(Fit.OneOut.Assault.Seas$coefficients[2]           + qt(.975, length(Fit.OneOut.Assault.Seas$residuals-1))          *summary(Fit.OneOut.Assault.Seas)$coef[2,2])          -100
Summary.Table.OneOut$UL.Seas[9]  <- 100*exp(Fit.OneOut.Burglary.Seas$coefficients[2]          + qt(.975, length(Fit.OneOut.Burglary.Seas$residuals-1))         *summary(Fit.OneOut.Burglary.Seas)$coef[2,2])         -100
Summary.Table.OneOut$UL.Seas[10] <- 100*exp(Fit.OneOut.Homicide.Seas$coefficients[2]          + qt(.975, length(Fit.OneOut.Homicide.Seas$residuals-1))         *summary(Fit.OneOut.Homicide.Seas)$coef[2,2])         -100
Summary.Table.OneOut$UL.Seas[11] <- 100*exp(Fit.OneOut.DeceptivePractice.Seas$coefficients[2] + qt(.975, length(Fit.OneOut.DeceptivePractice.Seas$residuals-1))*summary(Fit.OneOut.DeceptivePractice.Seas)$coef[2,2])-100
Summary.Table.OneOut$star.Seas[which(Summary.Table.OneOut$p.value.Seas<0.01)]                                           <- rep("**", length(which(Summary.Table.OneOut$p.value.Seas<0.01)))
Summary.Table.OneOut$star.Seas[which(Summary.Table.OneOut$p.value.Seas<0.05 & Summary.Table.OneOut$p.value.Seas>=0.01)] <- rep("*" , length(which(Summary.Table.OneOut$p.value.Seas<0.05 & Summary.Table.OneOut$p.value.Seas>=0.01)))



Street.Lights.AllOut.Pois$Month <- numeric(nrow(Street.Lights.AllOut.Pois))
Street.Lights.AllOut.Pois$Month[which(Street.Lights.AllOut.Pois$Time=="BEFORE")] <- as.numeric(substr(as.Date(Street.Lights.AllOut.Pois$DateCreated, "%m/%d/%Y")-22,6,7))[which(Street.Lights.AllOut.Pois$Time=="BEFORE")] 
Street.Lights.AllOut.Pois$Month[which(Street.Lights.AllOut.Pois$Time=="DURING")] <- as.numeric(substr(as.Date(Street.Lights.AllOut.Pois$DateCreated, "%m/%d/%Y")+round(Street.Lights.AllOut.Pois$Duration/2,0),6,7))[which(Street.Lights.AllOut.Pois$Time=="DURING")] 
Street.Lights.AllOut.Pois$Month[which(Street.Lights.AllOut.Pois$Time=="AFTER")]  <- as.numeric(substr(as.Date(Street.Lights.AllOut.Pois$DateCompleted, "%m/%d/%Y")+7+round(Street.Lights.AllOut.Pois$Duration/2,0),6,7))[which(Street.Lights.AllOut.Pois$Time=="AFTER")] 
Street.Lights.AllOut.Pois$Year <- numeric(nrow(Street.Lights.AllOut.Pois))
Street.Lights.AllOut.Pois$Year[which(Street.Lights.AllOut.Pois$Time=="BEFORE")] <- as.numeric(substr(as.Date(Street.Lights.AllOut.Pois$DateCreated, "%m/%d/%Y")-22,3,4))[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]  
Street.Lights.AllOut.Pois$Year[which(Street.Lights.AllOut.Pois$Time=="DURING")] <- as.numeric(substr(as.Date(Street.Lights.AllOut.Pois$DateCreated, "%m/%d/%Y")+round(Street.Lights.AllOut.Pois$Duration/2,0),3,4))[which(Street.Lights.AllOut.Pois$Time=="DURING")] 
Street.Lights.AllOut.Pois$Year[which(Street.Lights.AllOut.Pois$Time=="AFTER")]  <- as.numeric(substr(as.Date(Street.Lights.AllOut.Pois$DateCompleted, "%m/%d/%Y")+7+round(Street.Lights.AllOut.Pois$Duration/2,0),3,4))[which(Street.Lights.AllOut.Pois$Time=="AFTER")] 

Street.Lights.AllOut.Pois$Mar12 <- as.numeric(Street.Lights.AllOut.Pois$Month==3)  * as.numeric(Street.Lights.AllOut.Pois$Year==12)
Street.Lights.AllOut.Pois$Apr12 <- as.numeric(Street.Lights.AllOut.Pois$Month==4)  * as.numeric(Street.Lights.AllOut.Pois$Year==12)
Street.Lights.AllOut.Pois$May12 <- as.numeric(Street.Lights.AllOut.Pois$Month==5)  * as.numeric(Street.Lights.AllOut.Pois$Year==12)
Street.Lights.AllOut.Pois$Jun12 <- as.numeric(Street.Lights.AllOut.Pois$Month==6)  * as.numeric(Street.Lights.AllOut.Pois$Year==12)
Street.Lights.AllOut.Pois$Jul12 <- as.numeric(Street.Lights.AllOut.Pois$Month==7)  * as.numeric(Street.Lights.AllOut.Pois$Year==12) 
Street.Lights.AllOut.Pois$Aug12 <- as.numeric(Street.Lights.AllOut.Pois$Month==8)  * as.numeric(Street.Lights.AllOut.Pois$Year==12)
Street.Lights.AllOut.Pois$Sep12 <- as.numeric(Street.Lights.AllOut.Pois$Month==9)  * as.numeric(Street.Lights.AllOut.Pois$Year==12)
Street.Lights.AllOut.Pois$Oct12 <- as.numeric(Street.Lights.AllOut.Pois$Month==10) * as.numeric(Street.Lights.AllOut.Pois$Year==12)
Street.Lights.AllOut.Pois$Nov12 <- as.numeric(Street.Lights.AllOut.Pois$Month==11) * as.numeric(Street.Lights.AllOut.Pois$Year==12)
Street.Lights.AllOut.Pois$Dec12 <- as.numeric(Street.Lights.AllOut.Pois$Month==12) * as.numeric(Street.Lights.AllOut.Pois$Year==12)
Street.Lights.AllOut.Pois$Jan13 <- as.numeric(Street.Lights.AllOut.Pois$Month==1)  * as.numeric(Street.Lights.AllOut.Pois$Year==13)
Street.Lights.AllOut.Pois$Feb13 <- as.numeric(Street.Lights.AllOut.Pois$Month==2)  * as.numeric(Street.Lights.AllOut.Pois$Year==13)
Street.Lights.AllOut.Pois$Mar13 <- as.numeric(Street.Lights.AllOut.Pois$Month==3)  * as.numeric(Street.Lights.AllOut.Pois$Year==13)
Street.Lights.AllOut.Pois$Apr13 <- as.numeric(Street.Lights.AllOut.Pois$Month==4)  * as.numeric(Street.Lights.AllOut.Pois$Year==13)
Street.Lights.AllOut.Pois$May13 <- as.numeric(Street.Lights.AllOut.Pois$Month==5)  * as.numeric(Street.Lights.AllOut.Pois$Year==13)
Street.Lights.AllOut.Pois$Jun13 <- as.numeric(Street.Lights.AllOut.Pois$Month==6)  * as.numeric(Street.Lights.AllOut.Pois$Year==13)
Street.Lights.AllOut.Pois$Jul13 <- as.numeric(Street.Lights.AllOut.Pois$Month==7)  * as.numeric(Street.Lights.AllOut.Pois$Year==13)

Fit.AllOut.AllCrimes.Seas         <- glm(AllCrimes         ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Theft.Seas             <- glm(Theft             ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Narcotics.Seas         <- glm(Narcotics         ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Battery.Seas           <- glm(Battery           ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.CriminalDamage.Seas    <- glm(CriminalDamage    ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.MotorVehicleTheft.Seas <- glm(MotorVehicleTheft ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Robbery.Seas           <- glm(Robbery           ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Assault.Seas           <- glm(Assault           ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Burglary.Seas          <- glm(Burglary          ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Homicide.Seas          <- glm(Homicide          ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.DeceptivePractice.Seas <- glm(DeceptivePractice ~ offset(log(Duration)) + OutageInd + Apr12 + May12 + Jun12 + Jul12 + Aug12 + Sep12 + Oct12 + Nov12 + Dec12 + Jan13 + Feb13 + Mar13 + Apr13 + May13 + Jun13 + Jul13, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Summary.Table.AllOut$PctDiff.Seas <- numeric(nrow(Summary.Table.AllOut))
Summary.Table.AllOut$LL.Seas <- numeric(nrow(Summary.Table.AllOut))
Summary.Table.AllOut$UL.Seas <- numeric(nrow(Summary.Table.AllOut))
Summary.Table.AllOut$p.value.Seas <- numeric(nrow(Summary.Table.AllOut))
Summary.Table.AllOut$star.Seas <- character(nrow(Summary.Table.AllOut))
Summary.Table.AllOut$PctDiff.Seas[1] <- 100*exp(Fit.AllOut.AllCrimes.Seas$coef[2])-100
Summary.Table.AllOut$PctDiff.Seas[2] <- 100*exp(Fit.AllOut.Theft.Seas$coef[2])-100
Summary.Table.AllOut$PctDiff.Seas[3] <- 100*exp(Fit.AllOut.Narcotics.Seas$coef[2])-100
Summary.Table.AllOut$PctDiff.Seas[4] <- 100*exp(Fit.AllOut.Battery.Seas$coef[2])-100
Summary.Table.AllOut$PctDiff.Seas[5] <- 100*exp(Fit.AllOut.CriminalDamage.Seas$coef[2])-100
Summary.Table.AllOut$PctDiff.Seas[6] <- 100*exp(Fit.AllOut.MotorVehicleTheft.Seas$coef[2])-100
Summary.Table.AllOut$PctDiff.Seas[7] <- 100*exp(Fit.AllOut.Robbery.Seas$coef[2])-100
Summary.Table.AllOut$PctDiff.Seas[8] <- 100*exp(Fit.AllOut.Assault.Seas$coef[2])-100
Summary.Table.AllOut$PctDiff.Seas[9] <- 100*exp(Fit.AllOut.Burglary.Seas$coef[2])-100
Summary.Table.AllOut$PctDiff.Seas[10] <- 100*exp(Fit.AllOut.Homicide.Seas$coef[2])-100
Summary.Table.AllOut$PctDiff.Seas[11] <- 100*exp(Fit.AllOut.DeceptivePractice.Seas$coef[2])-100
Summary.Table.AllOut$p.value.Seas[1] <- summary(Fit.AllOut.AllCrimes.Seas)$coef[2,4]
Summary.Table.AllOut$p.value.Seas[2] <- summary(Fit.AllOut.Theft.Seas)$coef[2,4]
Summary.Table.AllOut$p.value.Seas[3] <- summary(Fit.AllOut.Narcotics.Seas)$coef[2,4]
Summary.Table.AllOut$p.value.Seas[4] <- summary(Fit.AllOut.Battery.Seas)$coef[2,4]
Summary.Table.AllOut$p.value.Seas[5] <- summary(Fit.AllOut.CriminalDamage.Seas)$coef[2,4]
Summary.Table.AllOut$p.value.Seas[6] <- summary(Fit.AllOut.MotorVehicleTheft.Seas)$coef[2,4]
Summary.Table.AllOut$p.value.Seas[7] <- summary(Fit.AllOut.Robbery.Seas)$coef[2,4]
Summary.Table.AllOut$p.value.Seas[8] <- summary(Fit.AllOut.Assault.Seas)$coef[2,4]
Summary.Table.AllOut$p.value.Seas[9] <- summary(Fit.AllOut.Burglary.Seas)$coef[2,4]
Summary.Table.AllOut$p.value.Seas[10] <- summary(Fit.AllOut.Homicide.Seas)$coef[2,4]
Summary.Table.AllOut$p.value.Seas[11] <- summary(Fit.AllOut.DeceptivePractice.Seas)$coef[2,4]
Summary.Table.AllOut$LL.Seas[1]  <- 100*exp(Fit.AllOut.AllCrimes.Seas$coefficients[2]         - qt(.975, length(Fit.AllOut.AllCrimes.Seas$residuals-1))        *summary(Fit.AllOut.AllCrimes.Seas)$coef[2,2])        -100
Summary.Table.AllOut$LL.Seas[2]  <- 100*exp(Fit.AllOut.Theft.Seas$coefficients[2]            - qt(.975, length(Fit.AllOut.Theft.Seas$residuals-1))           *summary(Fit.AllOut.Theft.Seas)$coef[2,2])           -100
Summary.Table.AllOut$LL.Seas[3]  <- 100*exp(Fit.AllOut.Narcotics.Seas$coefficients[2]         - qt(.975, length(Fit.AllOut.Narcotics.Seas$residuals-1))        *summary(Fit.AllOut.Narcotics.Seas)$coef[2,2])        -100
Summary.Table.AllOut$LL.Seas[4]  <- 100*exp(Fit.AllOut.Battery.Seas$coefficients[2]           - qt(.975, length(Fit.AllOut.Battery.Seas$residuals-1))          *summary(Fit.AllOut.Battery.Seas)$coef[2,2])          -100
Summary.Table.AllOut$LL.Seas[5]  <- 100*exp(Fit.AllOut.CriminalDamage.Seas$coefficients[2]    - qt(.975, length(Fit.AllOut.CriminalDamage.Seas$residuals-1))   *summary(Fit.AllOut.CriminalDamage.Seas)$coef[2,2])   -100
Summary.Table.AllOut$LL.Seas[6]  <- 100*exp(Fit.AllOut.MotorVehicleTheft.Seas$coefficients[2] - qt(.975, length(Fit.AllOut.MotorVehicleTheft.Seas$residuals-1))*summary(Fit.AllOut.MotorVehicleTheft.Seas)$coef[2,2])-100
Summary.Table.AllOut$LL.Seas[7]  <- 100*exp(Fit.AllOut.Robbery.Seas$coefficients[2]           - qt(.975, length(Fit.AllOut.Robbery.Seas$residuals-1))          *summary(Fit.AllOut.Robbery.Seas)$coef[2,2])          -100
Summary.Table.AllOut$LL.Seas[8]  <- 100*exp(Fit.AllOut.Assault.Seas$coefficients[2]           - qt(.975, length(Fit.AllOut.Assault.Seas$residuals-1))          *summary(Fit.AllOut.Assault.Seas)$coef[2,2])          -100
Summary.Table.AllOut$LL.Seas[9]  <- 100*exp(Fit.AllOut.Burglary.Seas$coefficients[2]          - qt(.975, length(Fit.AllOut.Burglary.Seas$residuals-1))         *summary(Fit.AllOut.Burglary.Seas)$coef[2,2])         -100
Summary.Table.AllOut$LL.Seas[10] <- 100*exp(Fit.AllOut.Homicide.Seas$coefficients[2]          - qt(.975, length(Fit.AllOut.Homicide.Seas$residuals-1))         *summary(Fit.AllOut.Homicide.Seas)$coef[2,2])         -100
Summary.Table.AllOut$LL.Seas[11] <- 100*exp(Fit.AllOut.DeceptivePractice.Seas$coefficients[2] - qt(.975, length(Fit.AllOut.DeceptivePractice.Seas$residuals-1))*summary(Fit.AllOut.DeceptivePractice.Seas)$coef[2,2])-100
Summary.Table.AllOut$UL.Seas[1]  <- 100*exp(Fit.AllOut.AllCrimes.Seas$coefficients[2]         + qt(.975, length(Fit.AllOut.AllCrimes.Seas$residuals-1))        *summary(Fit.AllOut.AllCrimes.Seas)$coef[2,2])        -100
Summary.Table.AllOut$UL.Seas[2]  <- 100*exp(Fit.AllOut.Theft.Seas$coefficients[2]            + qt(.975, length(Fit.AllOut.Theft.Seas$residuals-1))           *summary(Fit.AllOut.Theft.Seas)$coef[2,2])           -100
Summary.Table.AllOut$UL.Seas[3]  <- 100*exp(Fit.AllOut.Narcotics.Seas$coefficients[2]         + qt(.975, length(Fit.AllOut.Narcotics.Seas$residuals-1))        *summary(Fit.AllOut.Narcotics.Seas)$coef[2,2])        -100
Summary.Table.AllOut$UL.Seas[4]  <- 100*exp(Fit.AllOut.Battery.Seas$coefficients[2]           + qt(.975, length(Fit.AllOut.Battery.Seas$residuals-1))          *summary(Fit.AllOut.Battery.Seas)$coef[2,2])          -100
Summary.Table.AllOut$UL.Seas[5]  <- 100*exp(Fit.AllOut.CriminalDamage.Seas$coefficients[2]    + qt(.975, length(Fit.AllOut.CriminalDamage.Seas$residuals-1))   *summary(Fit.AllOut.CriminalDamage.Seas)$coef[2,2])   -100
Summary.Table.AllOut$UL.Seas[6]  <- 100*exp(Fit.AllOut.MotorVehicleTheft.Seas$coefficients[2] + qt(.975, length(Fit.AllOut.MotorVehicleTheft.Seas$residuals-1))*summary(Fit.AllOut.MotorVehicleTheft.Seas)$coef[2,2])-100
Summary.Table.AllOut$UL.Seas[7]  <- 100*exp(Fit.AllOut.Robbery.Seas$coefficients[2]           + qt(.975, length(Fit.AllOut.Robbery.Seas$residuals-1))          *summary(Fit.AllOut.Robbery.Seas)$coef[2,2])          -100
Summary.Table.AllOut$UL.Seas[8]  <- 100*exp(Fit.AllOut.Assault.Seas$coefficients[2]           + qt(.975, length(Fit.AllOut.Assault.Seas$residuals-1))          *summary(Fit.AllOut.Assault.Seas)$coef[2,2])          -100
Summary.Table.AllOut$UL.Seas[9]  <- 100*exp(Fit.AllOut.Burglary.Seas$coefficients[2]          + qt(.975, length(Fit.AllOut.Burglary.Seas$residuals-1))         *summary(Fit.AllOut.Burglary.Seas)$coef[2,2])         -100
Summary.Table.AllOut$UL.Seas[10] <- 100*exp(Fit.AllOut.Homicide.Seas$coefficients[2]          + qt(.975, length(Fit.AllOut.Homicide.Seas$residuals-1))         *summary(Fit.AllOut.Homicide.Seas)$coef[2,2])         -100
Summary.Table.AllOut$UL.Seas[11] <- 100*exp(Fit.AllOut.DeceptivePractice.Seas$coefficients[2] + qt(.975, length(Fit.AllOut.DeceptivePractice.Seas$residuals-1))*summary(Fit.AllOut.DeceptivePractice.Seas)$coef[2,2])-100
Summary.Table.AllOut$star.Seas[which(Summary.Table.AllOut$p.value.Seas<0.01)]                                           <- rep("**", length(which(Summary.Table.AllOut$p.value.Seas<0.01)))
Summary.Table.AllOut$star.Seas[which(Summary.Table.AllOut$p.value.Seas<0.05 & Summary.Table.AllOut$p.value.Seas>=0.01)] <- rep("*" , length(which(Summary.Table.AllOut$p.value.Seas<0.05 & Summary.Table.AllOut$p.value.Seas>=0.01)))


Summary.Table.Alley[,-c(10,15)]  <- round(Summary.Table.Alley[,-c(10,15)] , digits=3) 
Summary.Table.OneOut[,-c(10,15)] <- round(Summary.Table.OneOut[,-c(10,15)], digits=3)
Summary.Table.AllOut[,-c(10,15)] <- round(Summary.Table.AllOut[,-c(10,15)], digits=3)


# Export Tables for Community Area Level Estimates
write.csv(Alley.Lights.Pois, "Alley_Lights_and_Crime_for_Comm_Area_Est.csv")
write.csv(Street.Lights.OneOut.Pois, "Street_Lights_One_Out_and_Crime_for_Comm_Area_Est.csv")
write.csv(Street.Lights.AllOut.Pois, "Street_Lights_All_Out_and_Crime_for_Comm_Area_Est.csv")








