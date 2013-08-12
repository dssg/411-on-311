rm(list = ls())

#Load Necessary Packages
library(plyr)
library(doBy)
library(reshape)


setwd("C:/Users/Zach/Documents/UrbanCCD/Streetlights")

#Open Alley Lights and Crime
Alley.Lights<- read.csv(file="Alley_Lights_and_Crime.csv", head=TRUE)

#Open Street Lights One Out and Crime
Street.Lights.OneOut<- read.csv(file="Street_Lights_One_Out_and_Crime.csv", head=TRUE)

#Open Street Lights All Out and Crime
Street.Lights.AllOut<- read.csv(file="Street_Lights_All_Out_and_Crime.csv", head=TRUE)

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
                                                                              "Thefts", "Narcotics", "Battery", "Criminal Damage", "Motor Vehicle Theft", 
                                                                               "Robbery", "Assault", "Burglary", "Homicide", "Deceptive Practice"))
Summary.Table.OneOut <- data.frame(matrix(ncol = 4, nrow = 11), row.names = c("All Crimes (No Deceptive Practice)", 
                                                                              "Thefts", "Narcotics", "Battery", "Criminal Damage", "Motor Vehicle Theft", 
                                                                              "Robbery", "Assault", "Burglary", "Homicide", "Deceptive Practice"))
Summary.Table.AllOut <- data.frame(matrix(ncol = 4, nrow = 11), row.names = c("All Crimes (No Deceptive Practice)", 
                                                                              "Thefts", "Narcotics", "Battery", "Criminal Damage", "Motor Vehicle Theft", 
                                                                              "Robbery", "Assault", "Burglary", "Homicide", "Deceptive Practice"))
Summary.Table.Alley  <- rename(Summary.Table.Alley,  c("X1"="Before", "X2"="During", "X3"="After", "X4"="AvgBeforeAfter"))
Summary.Table.OneOut <- rename(Summary.Table.OneOut, c("X1"="Before", "X2"="During", "X3"="After", "X4"="AvgBeforeAfter"))
Summary.Table.AllOut <- rename(Summary.Table.AllOut, c("X1"="Before", "X2"="During", "X3"="After", "X4"="AvgBeforeAfter"))



Summary.Table.Alley[1,1]  <- 30*sum(Alley.Lights$Crimes.All.Before)       /(30*nrow(Alley.Lights))
Summary.Table.Alley[2,1]  <- 30*sum(Alley.Lights$Thefts.Before)           /(30*nrow(Alley.Lights))
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
Summary.Table.Alley[2,2]  <- 30*sum(Alley.Lights$Thefts.During)           /sum(Alley.Lights$OutageDuration)
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
Summary.Table.Alley[2,3]  <- 30*sum(Alley.Lights$Thefts.After)            /sum(Alley.Lights$After.Period.Duration)
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
Summary.Table.Alley[2,4]  <- 30*(sum(Alley.Lights$Thefts.Before)           +sum(Alley.Lights$Thefts.After))           /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
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
Summary.Table.OneOut[2,1]  <- 30*sum(Street.Lights.OneOut$Thefts.Before)           /(30*nrow(Street.Lights.OneOut))
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
Summary.Table.OneOut[2,2]  <- 30*sum(Street.Lights.OneOut$Thefts.During)           /sum(Street.Lights.OneOut$OutageDuration)
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
Summary.Table.OneOut[2,3]  <- 30*sum(Street.Lights.OneOut$Thefts.After)            /sum(Street.Lights.OneOut$After.Period.Duration)
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
Summary.Table.OneOut[2,4]  <- 30*(sum(Street.Lights.OneOut$Thefts.Before)           +sum(Street.Lights.OneOut$Thefts.After))           /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[3,4]  <- 30*(sum(Street.Lights.OneOut$Narcotics.Before)        +sum(Street.Lights.OneOut$Narcotics.After))        /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[4,4]  <- 30*(sum(Street.Lights.OneOut$Battery.Before)          +sum(Street.Lights.OneOut$Battery.After))          /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[5,4]  <- 30*(sum(Street.Lights.OneOut$CriminalDamage.Before)   +sum(Street.Lights.OneOut$CriminalDamage.After))   /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[6,4]  <- 30*(sum(Street.Lights.OneOut$MotorVehicleTheft.Before)+sum(Street.Lights.OneOut$MotorVehicleTheft.After))/(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[7,4]  <- 30*(sum(Street.Lights.OneOut$Robbery.Before)          +sum(Street.Lights.OneOut$Robbery.After))          /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[8,4] <- 30*(sum(Street.Lights.OneOut$Assault.Before)           +sum(Street.Lights.OneOut$Assault.After))          /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[9,4] <- 30*(sum(Street.Lights.OneOut$Burglary.Before)          +sum(Street.Lights.OneOut$Burglary.After))         /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[10,4] <- 30*(sum(Street.Lights.OneOut$Homicide.Before)         +sum(Street.Lights.OneOut$Homicide.After))         /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[11,4] <- 30*(sum(Street.Lights.OneOut$DeceptivePractice.Before)+sum(Street.Lights.OneOut$DeceptivePractice.After))/(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))



Summary.Table.AllOut[1,1]  <- 30*sum(Street.Lights.AllOut$Crimes.All.Before)       /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[2,1]  <- 30*sum(Street.Lights.AllOut$Thefts.Before)           /(30*nrow(Street.Lights.AllOut))
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
Summary.Table.AllOut[2,2]  <- 30*sum(Street.Lights.AllOut$Thefts.During)           /sum(Street.Lights.AllOut$OutageDuration)
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
Summary.Table.AllOut[2,3]  <- 30*sum(Street.Lights.AllOut$Thefts.After)            /sum(Street.Lights.AllOut$After.Period.Duration)
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
Summary.Table.AllOut[2,4]  <- 30*(sum(Street.Lights.AllOut$Thefts.Before)           +sum(Street.Lights.AllOut$Thefts.After))           /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
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


# Create Summary variables by Community Area
Alley.Lights$Number.Outages         <- rep(1, nrow(Alley.Lights))
Street.Lights.OneOut$Number.Outages <- rep(1, nrow(Street.Lights.OneOut))
Street.Lights.AllOut$Number.Outages <- rep(1, nrow(Street.Lights.AllOut))

Alley.Lights.CommArea <- summaryBy(Number.Outages + OutageDuration + After.Period.Duration + Thefts.During + Thefts.Before + Thefts.After + Narcotics.During +
                                   Narcotics.Before + Narcotics.After + Battery.During + Battery.Before + Battery.After + CriminalDamage.During + 
                                   CriminalDamage.Before + CriminalDamage.After + MotorVehicleTheft.During + MotorVehicleTheft.Before + 
                                   MotorVehicleTheft.After + Robbery.During + Robbery.Before + Robbery.After + Assault.During + Assault.Before + 
                                   Assault.After + Burglary.During + Burglary.Before + Burglary.After + Homicide.During + Homicide.Before + 
                                   Homicide.After + Crimes.All.During + Crimes.All.Before + Crimes.All.After ~ 
                                   community_area, FUN = c(sum), Alley.Lights)

Street.Lights.OneOut.CommArea <- summaryBy(Number.Outages + OutageDuration + After.Period.Duration + Thefts.During + Thefts.Before + Thefts.After + Narcotics.During +
                                           Narcotics.Before + Narcotics.After + Battery.During + Battery.Before + Battery.After + CriminalDamage.During + 
                                           CriminalDamage.Before + CriminalDamage.After + MotorVehicleTheft.During + MotorVehicleTheft.Before + 
                                           MotorVehicleTheft.After + Robbery.During + Robbery.Before + Robbery.After + Assault.During + Assault.Before + 
                                           Assault.After + Burglary.During + Burglary.Before + Burglary.After + Homicide.During + Homicide.Before + 
                                           Homicide.After + Crimes.All.During + Crimes.All.Before + Crimes.All.After ~ 
                                           community_area, FUN = c(sum), Street.Lights.OneOut)

Street.Lights.AllOut.CommArea <- summaryBy(Number.Outages + OutageDuration + After.Period.Duration + Thefts.During + Thefts.Before + Thefts.After + Narcotics.During +
                                           Narcotics.Before + Narcotics.After + Battery.During + Battery.Before + Battery.After + CriminalDamage.During + 
                                           CriminalDamage.Before + CriminalDamage.After + MotorVehicleTheft.During + MotorVehicleTheft.Before + 
                                           MotorVehicleTheft.After + Robbery.During + Robbery.Before + Robbery.After + Assault.During + Assault.Before + 
                                           Assault.After + Burglary.During + Burglary.Before + Burglary.After + Homicide.During + Homicide.Before + 
                                           Homicide.After + Crimes.All.During + Crimes.All.Before + Crimes.All.After ~ 
                                           community_area, FUN = c(sum), Street.Lights.AllOut)

Alley.Lights.CommArea$Before.Period.Duration.sum         = rep(30, nrow(Alley.Lights.CommArea))        *Alley.Lights.CommArea$Number.Outages.sum
Street.Lights.OneOut.CommArea$Before.Period.Duration.sum = rep(30, nrow(Street.Lights.OneOut.CommArea))*Street.Lights.OneOut.CommArea$Number.Outages.sum
Street.Lights.AllOut.CommArea$Before.Period.Duration.sum = rep(30, nrow(Street.Lights.AllOut.CommArea))*Street.Lights.AllOut.CommArea$Number.Outages.sum

Alley.Lights.CommArea$Rate.AllCrimes.Before         <- Alley.Lights.CommArea$Crimes.All.Before.sum       /Alley.Lights.CommArea$Before.Period.Duration.sum
Alley.Lights.CommArea$Rate.Thefts.Before            <- Alley.Lights.CommArea$Thefts.Before.sum           /Alley.Lights.CommArea$Before.Period.Duration.sum
Alley.Lights.CommArea$Rate.Narcotics.Before         <- Alley.Lights.CommArea$Narcotics.Before.sum        /Alley.Lights.CommArea$Before.Period.Duration.sum
Alley.Lights.CommArea$Rate.Battery.Before           <- Alley.Lights.CommArea$Battery.Before.sum          /Alley.Lights.CommArea$Before.Period.Duration.sum
Alley.Lights.CommArea$Rate.CriminalDamage.Before    <- Alley.Lights.CommArea$CriminalDamage.Before.sum   /Alley.Lights.CommArea$Before.Period.Duration.sum
Alley.Lights.CommArea$Rate.MotorVehicleTheft.Before <- Alley.Lights.CommArea$MotorVehicleTheft.Before.sum/Alley.Lights.CommArea$Before.Period.Duration.sum
Alley.Lights.CommArea$Rate.Robbery.Before           <- Alley.Lights.CommArea$Robbery.Before.sum          /Alley.Lights.CommArea$Before.Period.Duration.sum
Alley.Lights.CommArea$Rate.Assault.Before           <- Alley.Lights.CommArea$Assault.Before.sum          /Alley.Lights.CommArea$Before.Period.Duration.sum
Alley.Lights.CommArea$Rate.Burglary.Before          <- Alley.Lights.CommArea$Burglary.Before.sum         /Alley.Lights.CommArea$Before.Period.Duration.sum
Alley.Lights.CommArea$Rate.Homicide.Before          <- Alley.Lights.CommArea$Homicide.Before.sum         /Alley.Lights.CommArea$Before.Period.Duration.sum
Alley.Lights.CommArea$Rate.AllCrimes.During         <- Alley.Lights.CommArea$Crimes.All.During.sum       /Alley.Lights.CommArea$OutageDuration.sum
Alley.Lights.CommArea$Rate.Thefts.During            <- Alley.Lights.CommArea$Thefts.During.sum           /Alley.Lights.CommArea$OutageDuration.sum
Alley.Lights.CommArea$Rate.Narcotics.During         <- Alley.Lights.CommArea$Narcotics.During.sum        /Alley.Lights.CommArea$OutageDuration.sum
Alley.Lights.CommArea$Rate.Battery.During           <- Alley.Lights.CommArea$Battery.During.sum          /Alley.Lights.CommArea$OutageDuration.sum
Alley.Lights.CommArea$Rate.CriminalDamage.During    <- Alley.Lights.CommArea$CriminalDamage.During.sum   /Alley.Lights.CommArea$OutageDuration.sum
Alley.Lights.CommArea$Rate.MotorVehicleTheft.During <- Alley.Lights.CommArea$MotorVehicleTheft.During.sum/Alley.Lights.CommArea$OutageDuration.sum
Alley.Lights.CommArea$Rate.Robbery.During           <- Alley.Lights.CommArea$Robbery.During.sum          /Alley.Lights.CommArea$OutageDuration.sum
Alley.Lights.CommArea$Rate.Assault.During           <- Alley.Lights.CommArea$Assault.During.sum          /Alley.Lights.CommArea$OutageDuration.sum
Alley.Lights.CommArea$Rate.Burglary.During          <- Alley.Lights.CommArea$Burglary.During.sum         /Alley.Lights.CommArea$OutageDuration.sum
Alley.Lights.CommArea$Rate.Homicide.During          <- Alley.Lights.CommArea$Homicide.During.sum         /Alley.Lights.CommArea$OutageDuration.sum
Alley.Lights.CommArea$Rate.AllCrimes.After          <- Alley.Lights.CommArea$Crimes.All.After.sum        /Alley.Lights.CommArea$After.Period.Duration.sum
Alley.Lights.CommArea$Rate.Thefts.After             <- Alley.Lights.CommArea$Thefts.After.sum            /Alley.Lights.CommArea$After.Period.Duration.sum
Alley.Lights.CommArea$Rate.Narcotics.After          <- Alley.Lights.CommArea$Narcotics.After.sum         /Alley.Lights.CommArea$After.Period.Duration.sum
Alley.Lights.CommArea$Rate.Battery.After            <- Alley.Lights.CommArea$Battery.After.sum           /Alley.Lights.CommArea$After.Period.Duration.sum
Alley.Lights.CommArea$Rate.CriminalDamage.After     <- Alley.Lights.CommArea$CriminalDamage.After.sum    /Alley.Lights.CommArea$After.Period.Duration.sum
Alley.Lights.CommArea$Rate.MotorVehicleTheft.After  <- Alley.Lights.CommArea$MotorVehicleTheft.After.sum /Alley.Lights.CommArea$After.Period.Duration.sum
Alley.Lights.CommArea$Rate.Robbery.After            <- Alley.Lights.CommArea$Robbery.After.sum           /Alley.Lights.CommArea$After.Period.Duration.sum
Alley.Lights.CommArea$Rate.Assault.After            <- Alley.Lights.CommArea$Assault.After.sum           /Alley.Lights.CommArea$After.Period.Duration.sum
Alley.Lights.CommArea$Rate.Burglary.After           <- Alley.Lights.CommArea$Burglary.After.sum          /Alley.Lights.CommArea$After.Period.Duration.sum
Alley.Lights.CommArea$Rate.Homicide.After           <- Alley.Lights.CommArea$Homicide.After.sum          /Alley.Lights.CommArea$After.Period.Duration.sum
Alley.Lights.CommArea$Rate.AllCrimes.BeforeAfter         <- (Alley.Lights.CommArea$Crimes.All.Before.sum       +Alley.Lights.CommArea$Crimes.All.After.sum)       /(Alley.Lights.CommArea$Before.Period.Duration.sum+Alley.Lights.CommArea$After.Period.Duration.sum)
Alley.Lights.CommArea$Rate.Thefts.BeforeAfter            <- (Alley.Lights.CommArea$Thefts.Before.sum           +Alley.Lights.CommArea$Thefts.After.sum)           /(Alley.Lights.CommArea$Before.Period.Duration.sum+Alley.Lights.CommArea$After.Period.Duration.sum)
Alley.Lights.CommArea$Rate.Narcotics.BeforeAfter         <- (Alley.Lights.CommArea$Narcotics.Before.sum        +Alley.Lights.CommArea$Narcotics.After.sum)        /(Alley.Lights.CommArea$Before.Period.Duration.sum+Alley.Lights.CommArea$After.Period.Duration.sum)
Alley.Lights.CommArea$Rate.Battery.BeforeAfter           <- (Alley.Lights.CommArea$Battery.Before.sum          +Alley.Lights.CommArea$Battery.After.sum)          /(Alley.Lights.CommArea$Before.Period.Duration.sum+Alley.Lights.CommArea$After.Period.Duration.sum)
Alley.Lights.CommArea$Rate.CriminalDamage.BeforeAfter    <- (Alley.Lights.CommArea$CriminalDamage.Before.sum   +Alley.Lights.CommArea$CriminalDamage.After.sum)   /(Alley.Lights.CommArea$Before.Period.Duration.sum+Alley.Lights.CommArea$After.Period.Duration.sum)
Alley.Lights.CommArea$Rate.MotorVehicleTheft.BeforeAfter <- (Alley.Lights.CommArea$MotorVehicleTheft.Before.sum+Alley.Lights.CommArea$MotorVehicleTheft.After.sum)/(Alley.Lights.CommArea$Before.Period.Duration.sum+Alley.Lights.CommArea$After.Period.Duration.sum)
Alley.Lights.CommArea$Rate.Robbery.BeforeAfter           <- (Alley.Lights.CommArea$Robbery.Before.sum          +Alley.Lights.CommArea$Robbery.After.sum)          /(Alley.Lights.CommArea$Before.Period.Duration.sum+Alley.Lights.CommArea$After.Period.Duration.sum)
Alley.Lights.CommArea$Rate.Assault.BeforeAfter           <- (Alley.Lights.CommArea$Assault.Before.sum          +Alley.Lights.CommArea$Assault.After.sum)          /(Alley.Lights.CommArea$Before.Period.Duration.sum+Alley.Lights.CommArea$After.Period.Duration.sum)
Alley.Lights.CommArea$Rate.Burglary.BeforeAfter          <- (Alley.Lights.CommArea$Burglary.Before.sum         +Alley.Lights.CommArea$Burglary.After.sum)         /(Alley.Lights.CommArea$Before.Period.Duration.sum+Alley.Lights.CommArea$After.Period.Duration.sum)
Alley.Lights.CommArea$Rate.Homicide.BeforeAfter          <- (Alley.Lights.CommArea$Homicide.Before.sum         +Alley.Lights.CommArea$Homicide.After.sum)         /(Alley.Lights.CommArea$Before.Period.Duration.sum+Alley.Lights.CommArea$After.Period.Duration.sum)
Alley.Lights.CommArea$AbsDiff.AllCrimes         <- Alley.Lights.CommArea$Rate.AllCrimes.During         - Alley.Lights.CommArea$Rate.AllCrimes.BeforeAfter 
Alley.Lights.CommArea$AbsDiff.Thefts            <- Alley.Lights.CommArea$Rate.Thefts.During            - Alley.Lights.CommArea$Rate.Thefts.BeforeAfter 
Alley.Lights.CommArea$AbsDiff.Narcotics         <- Alley.Lights.CommArea$Rate.Narcotics.During         - Alley.Lights.CommArea$Rate.Narcotics.BeforeAfter 
Alley.Lights.CommArea$AbsDiff.Battery           <- Alley.Lights.CommArea$Rate.Battery.During           - Alley.Lights.CommArea$Rate.Battery.BeforeAfter 
Alley.Lights.CommArea$AbsDiff.CriminalDamage    <- Alley.Lights.CommArea$Rate.CriminalDamage.During    - Alley.Lights.CommArea$Rate.CriminalDamage.BeforeAfter 
Alley.Lights.CommArea$AbsDiff.MotorVehicleTheft <- Alley.Lights.CommArea$Rate.MotorVehicleTheft.During - Alley.Lights.CommArea$Rate.MotorVehicleTheft.BeforeAfter 
Alley.Lights.CommArea$AbsDiff.Robbery           <- Alley.Lights.CommArea$Rate.Robbery.During           - Alley.Lights.CommArea$Rate.Robbery.BeforeAfter 
Alley.Lights.CommArea$AbsDiff.Assault           <- Alley.Lights.CommArea$Rate.Assault.During           - Alley.Lights.CommArea$Rate.Assault.BeforeAfter 
Alley.Lights.CommArea$AbsDiff.Burglary          <- Alley.Lights.CommArea$Rate.Burglary.During          - Alley.Lights.CommArea$Rate.Burglary.BeforeAfter 
Alley.Lights.CommArea$AbsDiff.Homicide          <- Alley.Lights.CommArea$Rate.Homicide.During          - Alley.Lights.CommArea$Rate.Homicide.BeforeAfter 
Alley.Lights.CommArea$PctDiff.AllCrimes          <- 100*Alley.Lights.CommArea$AbsDiff.AllCrimes        / Alley.Lights.CommArea$Rate.AllCrimes.BeforeAfter
Alley.Lights.CommArea$PctDiff.Thefts             <- 100*Alley.Lights.CommArea$AbsDiff.Thefts           / Alley.Lights.CommArea$Rate.Thefts.BeforeAfter
Alley.Lights.CommArea$PctDiff.Narcotics          <- 100*Alley.Lights.CommArea$AbsDiff.Narcotics        / Alley.Lights.CommArea$Rate.Narcotics.BeforeAfter
Alley.Lights.CommArea$PctDiff.Battery            <- 100*Alley.Lights.CommArea$AbsDiff.Battery          / Alley.Lights.CommArea$Rate.Battery.BeforeAfter
Alley.Lights.CommArea$PctDiff.CriminalDamage     <- 100*Alley.Lights.CommArea$AbsDiff.CriminalDamage   / Alley.Lights.CommArea$Rate.CriminalDamage.BeforeAfter
Alley.Lights.CommArea$PctDiff.MotorVehicleTheft  <- 100*Alley.Lights.CommArea$AbsDiff.MotorVehicleTheft/ Alley.Lights.CommArea$Rate.MotorVehicleTheft.BeforeAfter
Alley.Lights.CommArea$PctDiff.Robbery            <- 100*Alley.Lights.CommArea$AbsDiff.Robbery          / Alley.Lights.CommArea$Rate.Robbery.BeforeAfter
Alley.Lights.CommArea$PctDiff.Assault            <- 100*Alley.Lights.CommArea$AbsDiff.Assault          / Alley.Lights.CommArea$Rate.Assault.BeforeAfter
Alley.Lights.CommArea$PctDiff.Burglary           <- 100*Alley.Lights.CommArea$AbsDiff.Burglary         / Alley.Lights.CommArea$Rate.Burglary.BeforeAfter
Alley.Lights.CommArea$PctDiff.Homicide           <- 100*Alley.Lights.CommArea$AbsDiff.Homicide         / Alley.Lights.CommArea$Rate.Homicide.BeforeAfter

Street.Lights.OneOut.CommArea$Rate.AllCrimes.Before         <- Street.Lights.OneOut.CommArea$Crimes.All.Before.sum       /Street.Lights.OneOut.CommArea$Before.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.Thefts.Before            <- Street.Lights.OneOut.CommArea$Thefts.Before.sum           /Street.Lights.OneOut.CommArea$Before.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.Narcotics.Before         <- Street.Lights.OneOut.CommArea$Narcotics.Before.sum        /Street.Lights.OneOut.CommArea$Before.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.Battery.Before           <- Street.Lights.OneOut.CommArea$Battery.Before.sum          /Street.Lights.OneOut.CommArea$Before.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.CriminalDamage.Before    <- Street.Lights.OneOut.CommArea$CriminalDamage.Before.sum   /Street.Lights.OneOut.CommArea$Before.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.MotorVehicleTheft.Before <- Street.Lights.OneOut.CommArea$MotorVehicleTheft.Before.sum/Street.Lights.OneOut.CommArea$Before.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.Robbery.Before           <- Street.Lights.OneOut.CommArea$Robbery.Before.sum          /Street.Lights.OneOut.CommArea$Before.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.Assault.Before           <- Street.Lights.OneOut.CommArea$Assault.Before.sum          /Street.Lights.OneOut.CommArea$Before.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.Burglary.Before          <- Street.Lights.OneOut.CommArea$Burglary.Before.sum         /Street.Lights.OneOut.CommArea$Before.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.Homicide.Before          <- Street.Lights.OneOut.CommArea$Homicide.Before.sum         /Street.Lights.OneOut.CommArea$Before.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.AllCrimes.During         <- Street.Lights.OneOut.CommArea$Crimes.All.During.sum       /Street.Lights.OneOut.CommArea$OutageDuration.sum
Street.Lights.OneOut.CommArea$Rate.Thefts.During            <- Street.Lights.OneOut.CommArea$Thefts.During.sum           /Street.Lights.OneOut.CommArea$OutageDuration.sum
Street.Lights.OneOut.CommArea$Rate.Narcotics.During         <- Street.Lights.OneOut.CommArea$Narcotics.During.sum        /Street.Lights.OneOut.CommArea$OutageDuration.sum
Street.Lights.OneOut.CommArea$Rate.Battery.During           <- Street.Lights.OneOut.CommArea$Battery.During.sum          /Street.Lights.OneOut.CommArea$OutageDuration.sum
Street.Lights.OneOut.CommArea$Rate.CriminalDamage.During    <- Street.Lights.OneOut.CommArea$CriminalDamage.During.sum   /Street.Lights.OneOut.CommArea$OutageDuration.sum
Street.Lights.OneOut.CommArea$Rate.MotorVehicleTheft.During <- Street.Lights.OneOut.CommArea$MotorVehicleTheft.During.sum/Street.Lights.OneOut.CommArea$OutageDuration.sum
Street.Lights.OneOut.CommArea$Rate.Robbery.During           <- Street.Lights.OneOut.CommArea$Robbery.During.sum          /Street.Lights.OneOut.CommArea$OutageDuration.sum
Street.Lights.OneOut.CommArea$Rate.Assault.During           <- Street.Lights.OneOut.CommArea$Assault.During.sum          /Street.Lights.OneOut.CommArea$OutageDuration.sum
Street.Lights.OneOut.CommArea$Rate.Burglary.During          <- Street.Lights.OneOut.CommArea$Burglary.During.sum         /Street.Lights.OneOut.CommArea$OutageDuration.sum
Street.Lights.OneOut.CommArea$Rate.Homicide.During          <- Street.Lights.OneOut.CommArea$Homicide.During.sum         /Street.Lights.OneOut.CommArea$OutageDuration.sum
Street.Lights.OneOut.CommArea$Rate.AllCrimes.After          <- Street.Lights.OneOut.CommArea$Crimes.All.After.sum        /Street.Lights.OneOut.CommArea$After.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.Thefts.After             <- Street.Lights.OneOut.CommArea$Thefts.After.sum            /Street.Lights.OneOut.CommArea$After.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.Narcotics.After          <- Street.Lights.OneOut.CommArea$Narcotics.After.sum         /Street.Lights.OneOut.CommArea$After.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.Battery.After            <- Street.Lights.OneOut.CommArea$Battery.After.sum           /Street.Lights.OneOut.CommArea$After.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.CriminalDamage.After     <- Street.Lights.OneOut.CommArea$CriminalDamage.After.sum    /Street.Lights.OneOut.CommArea$After.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.MotorVehicleTheft.After  <- Street.Lights.OneOut.CommArea$MotorVehicleTheft.After.sum /Street.Lights.OneOut.CommArea$After.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.Robbery.After            <- Street.Lights.OneOut.CommArea$Robbery.After.sum           /Street.Lights.OneOut.CommArea$After.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.Assault.After            <- Street.Lights.OneOut.CommArea$Assault.After.sum           /Street.Lights.OneOut.CommArea$After.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.Burglary.After           <- Street.Lights.OneOut.CommArea$Burglary.After.sum          /Street.Lights.OneOut.CommArea$After.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.Homicide.After           <- Street.Lights.OneOut.CommArea$Homicide.After.sum          /Street.Lights.OneOut.CommArea$After.Period.Duration.sum
Street.Lights.OneOut.CommArea$Rate.AllCrimes.BeforeAfter         <- (Street.Lights.OneOut.CommArea$Crimes.All.Before.sum       +Street.Lights.OneOut.CommArea$Crimes.All.After.sum)       /(Street.Lights.OneOut.CommArea$Before.Period.Duration.sum+Street.Lights.OneOut.CommArea$After.Period.Duration.sum)
Street.Lights.OneOut.CommArea$Rate.Thefts.BeforeAfter            <- (Street.Lights.OneOut.CommArea$Thefts.Before.sum           +Street.Lights.OneOut.CommArea$Thefts.After.sum)           /(Street.Lights.OneOut.CommArea$Before.Period.Duration.sum+Street.Lights.OneOut.CommArea$After.Period.Duration.sum)
Street.Lights.OneOut.CommArea$Rate.Narcotics.BeforeAfter         <- (Street.Lights.OneOut.CommArea$Narcotics.Before.sum        +Street.Lights.OneOut.CommArea$Narcotics.After.sum)        /(Street.Lights.OneOut.CommArea$Before.Period.Duration.sum+Street.Lights.OneOut.CommArea$After.Period.Duration.sum)
Street.Lights.OneOut.CommArea$Rate.Battery.BeforeAfter           <- (Street.Lights.OneOut.CommArea$Battery.Before.sum          +Street.Lights.OneOut.CommArea$Battery.After.sum)          /(Street.Lights.OneOut.CommArea$Before.Period.Duration.sum+Street.Lights.OneOut.CommArea$After.Period.Duration.sum)
Street.Lights.OneOut.CommArea$Rate.CriminalDamage.BeforeAfter    <- (Street.Lights.OneOut.CommArea$CriminalDamage.Before.sum   +Street.Lights.OneOut.CommArea$CriminalDamage.After.sum)   /(Street.Lights.OneOut.CommArea$Before.Period.Duration.sum+Street.Lights.OneOut.CommArea$After.Period.Duration.sum)
Street.Lights.OneOut.CommArea$Rate.MotorVehicleTheft.BeforeAfter <- (Street.Lights.OneOut.CommArea$MotorVehicleTheft.Before.sum+Street.Lights.OneOut.CommArea$MotorVehicleTheft.After.sum)/(Street.Lights.OneOut.CommArea$Before.Period.Duration.sum+Street.Lights.OneOut.CommArea$After.Period.Duration.sum)
Street.Lights.OneOut.CommArea$Rate.Robbery.BeforeAfter           <- (Street.Lights.OneOut.CommArea$Robbery.Before.sum          +Street.Lights.OneOut.CommArea$Robbery.After.sum)          /(Street.Lights.OneOut.CommArea$Before.Period.Duration.sum+Street.Lights.OneOut.CommArea$After.Period.Duration.sum)
Street.Lights.OneOut.CommArea$Rate.Assault.BeforeAfter           <- (Street.Lights.OneOut.CommArea$Assault.Before.sum          +Street.Lights.OneOut.CommArea$Assault.After.sum)          /(Street.Lights.OneOut.CommArea$Before.Period.Duration.sum+Street.Lights.OneOut.CommArea$After.Period.Duration.sum)
Street.Lights.OneOut.CommArea$Rate.Burglary.BeforeAfter          <- (Street.Lights.OneOut.CommArea$Burglary.Before.sum         +Street.Lights.OneOut.CommArea$Burglary.After.sum)         /(Street.Lights.OneOut.CommArea$Before.Period.Duration.sum+Street.Lights.OneOut.CommArea$After.Period.Duration.sum)
Street.Lights.OneOut.CommArea$Rate.Homicide.BeforeAfter          <- (Street.Lights.OneOut.CommArea$Homicide.Before.sum         +Street.Lights.OneOut.CommArea$Homicide.After.sum)         /(Street.Lights.OneOut.CommArea$Before.Period.Duration.sum+Street.Lights.OneOut.CommArea$After.Period.Duration.sum)
Street.Lights.OneOut.CommArea$AbsDiff.AllCrimes         <- Street.Lights.OneOut.CommArea$Rate.AllCrimes.During         - Street.Lights.OneOut.CommArea$Rate.AllCrimes.BeforeAfter 
Street.Lights.OneOut.CommArea$AbsDiff.Thefts            <- Street.Lights.OneOut.CommArea$Rate.Thefts.During            - Street.Lights.OneOut.CommArea$Rate.Thefts.BeforeAfter 
Street.Lights.OneOut.CommArea$AbsDiff.Narcotics         <- Street.Lights.OneOut.CommArea$Rate.Narcotics.During         - Street.Lights.OneOut.CommArea$Rate.Narcotics.BeforeAfter 
Street.Lights.OneOut.CommArea$AbsDiff.Battery           <- Street.Lights.OneOut.CommArea$Rate.Battery.During           - Street.Lights.OneOut.CommArea$Rate.Battery.BeforeAfter 
Street.Lights.OneOut.CommArea$AbsDiff.CriminalDamage    <- Street.Lights.OneOut.CommArea$Rate.CriminalDamage.During    - Street.Lights.OneOut.CommArea$Rate.CriminalDamage.BeforeAfter 
Street.Lights.OneOut.CommArea$AbsDiff.MotorVehicleTheft <- Street.Lights.OneOut.CommArea$Rate.MotorVehicleTheft.During - Street.Lights.OneOut.CommArea$Rate.MotorVehicleTheft.BeforeAfter 
Street.Lights.OneOut.CommArea$AbsDiff.Robbery           <- Street.Lights.OneOut.CommArea$Rate.Robbery.During           - Street.Lights.OneOut.CommArea$Rate.Robbery.BeforeAfter 
Street.Lights.OneOut.CommArea$AbsDiff.Assault           <- Street.Lights.OneOut.CommArea$Rate.Assault.During           - Street.Lights.OneOut.CommArea$Rate.Assault.BeforeAfter 
Street.Lights.OneOut.CommArea$AbsDiff.Burglary          <- Street.Lights.OneOut.CommArea$Rate.Burglary.During          - Street.Lights.OneOut.CommArea$Rate.Burglary.BeforeAfter 
Street.Lights.OneOut.CommArea$AbsDiff.Homicide          <- Street.Lights.OneOut.CommArea$Rate.Homicide.During          - Street.Lights.OneOut.CommArea$Rate.Homicide.BeforeAfter 
Street.Lights.OneOut.CommArea$PctDiff.AllCrimes          <- 100*Street.Lights.OneOut.CommArea$AbsDiff.AllCrimes        / Street.Lights.OneOut.CommArea$Rate.AllCrimes.BeforeAfter
Street.Lights.OneOut.CommArea$PctDiff.Thefts             <- 100*Street.Lights.OneOut.CommArea$AbsDiff.Thefts           / Street.Lights.OneOut.CommArea$Rate.Thefts.BeforeAfter
Street.Lights.OneOut.CommArea$PctDiff.Narcotics          <- 100*Street.Lights.OneOut.CommArea$AbsDiff.Narcotics        / Street.Lights.OneOut.CommArea$Rate.Narcotics.BeforeAfter
Street.Lights.OneOut.CommArea$PctDiff.Battery            <- 100*Street.Lights.OneOut.CommArea$AbsDiff.Battery          / Street.Lights.OneOut.CommArea$Rate.Battery.BeforeAfter
Street.Lights.OneOut.CommArea$PctDiff.CriminalDamage     <- 100*Street.Lights.OneOut.CommArea$AbsDiff.CriminalDamage   / Street.Lights.OneOut.CommArea$Rate.CriminalDamage.BeforeAfter
Street.Lights.OneOut.CommArea$PctDiff.MotorVehicleTheft  <- 100*Street.Lights.OneOut.CommArea$AbsDiff.MotorVehicleTheft/ Street.Lights.OneOut.CommArea$Rate.MotorVehicleTheft.BeforeAfter
Street.Lights.OneOut.CommArea$PctDiff.Robbery            <- 100*Street.Lights.OneOut.CommArea$AbsDiff.Robbery          / Street.Lights.OneOut.CommArea$Rate.Robbery.BeforeAfter
Street.Lights.OneOut.CommArea$PctDiff.Assault            <- 100*Street.Lights.OneOut.CommArea$AbsDiff.Assault          / Street.Lights.OneOut.CommArea$Rate.Assault.BeforeAfter
Street.Lights.OneOut.CommArea$PctDiff.Burglary           <- 100*Street.Lights.OneOut.CommArea$AbsDiff.Burglary         / Street.Lights.OneOut.CommArea$Rate.Burglary.BeforeAfter
Street.Lights.OneOut.CommArea$PctDiff.Homicide           <- 100*Street.Lights.OneOut.CommArea$AbsDiff.Homicide         / Street.Lights.OneOut.CommArea$Rate.Homicide.BeforeAfter

Street.Lights.AllOut.CommArea$Rate.AllCrimes.Before         <- Street.Lights.AllOut.CommArea$Crimes.All.Before.sum       /Street.Lights.AllOut.CommArea$Before.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.Thefts.Before            <- Street.Lights.AllOut.CommArea$Thefts.Before.sum           /Street.Lights.AllOut.CommArea$Before.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.Narcotics.Before         <- Street.Lights.AllOut.CommArea$Narcotics.Before.sum        /Street.Lights.AllOut.CommArea$Before.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.Battery.Before           <- Street.Lights.AllOut.CommArea$Battery.Before.sum          /Street.Lights.AllOut.CommArea$Before.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.CriminalDamage.Before    <- Street.Lights.AllOut.CommArea$CriminalDamage.Before.sum   /Street.Lights.AllOut.CommArea$Before.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.MotorVehicleTheft.Before <- Street.Lights.AllOut.CommArea$MotorVehicleTheft.Before.sum/Street.Lights.AllOut.CommArea$Before.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.Robbery.Before           <- Street.Lights.AllOut.CommArea$Robbery.Before.sum          /Street.Lights.AllOut.CommArea$Before.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.Assault.Before           <- Street.Lights.AllOut.CommArea$Assault.Before.sum          /Street.Lights.AllOut.CommArea$Before.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.Burglary.Before          <- Street.Lights.AllOut.CommArea$Burglary.Before.sum         /Street.Lights.AllOut.CommArea$Before.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.Homicide.Before          <- Street.Lights.AllOut.CommArea$Homicide.Before.sum         /Street.Lights.AllOut.CommArea$Before.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.AllCrimes.During         <- Street.Lights.AllOut.CommArea$Crimes.All.During.sum       /Street.Lights.AllOut.CommArea$OutageDuration.sum
Street.Lights.AllOut.CommArea$Rate.Thefts.During            <- Street.Lights.AllOut.CommArea$Thefts.During.sum           /Street.Lights.AllOut.CommArea$OutageDuration.sum
Street.Lights.AllOut.CommArea$Rate.Narcotics.During         <- Street.Lights.AllOut.CommArea$Narcotics.During.sum        /Street.Lights.AllOut.CommArea$OutageDuration.sum
Street.Lights.AllOut.CommArea$Rate.Battery.During           <- Street.Lights.AllOut.CommArea$Battery.During.sum          /Street.Lights.AllOut.CommArea$OutageDuration.sum
Street.Lights.AllOut.CommArea$Rate.CriminalDamage.During    <- Street.Lights.AllOut.CommArea$CriminalDamage.During.sum   /Street.Lights.AllOut.CommArea$OutageDuration.sum
Street.Lights.AllOut.CommArea$Rate.MotorVehicleTheft.During <- Street.Lights.AllOut.CommArea$MotorVehicleTheft.During.sum/Street.Lights.AllOut.CommArea$OutageDuration.sum
Street.Lights.AllOut.CommArea$Rate.Robbery.During           <- Street.Lights.AllOut.CommArea$Robbery.During.sum          /Street.Lights.AllOut.CommArea$OutageDuration.sum
Street.Lights.AllOut.CommArea$Rate.Assault.During           <- Street.Lights.AllOut.CommArea$Assault.During.sum          /Street.Lights.AllOut.CommArea$OutageDuration.sum
Street.Lights.AllOut.CommArea$Rate.Burglary.During          <- Street.Lights.AllOut.CommArea$Burglary.During.sum         /Street.Lights.AllOut.CommArea$OutageDuration.sum
Street.Lights.AllOut.CommArea$Rate.Homicide.During          <- Street.Lights.AllOut.CommArea$Homicide.During.sum         /Street.Lights.AllOut.CommArea$OutageDuration.sum
Street.Lights.AllOut.CommArea$Rate.AllCrimes.After          <- Street.Lights.AllOut.CommArea$Crimes.All.After.sum        /Street.Lights.AllOut.CommArea$After.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.Thefts.After             <- Street.Lights.AllOut.CommArea$Thefts.After.sum            /Street.Lights.AllOut.CommArea$After.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.Narcotics.After          <- Street.Lights.AllOut.CommArea$Narcotics.After.sum         /Street.Lights.AllOut.CommArea$After.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.Battery.After            <- Street.Lights.AllOut.CommArea$Battery.After.sum           /Street.Lights.AllOut.CommArea$After.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.CriminalDamage.After     <- Street.Lights.AllOut.CommArea$CriminalDamage.After.sum    /Street.Lights.AllOut.CommArea$After.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.MotorVehicleTheft.After  <- Street.Lights.AllOut.CommArea$MotorVehicleTheft.After.sum /Street.Lights.AllOut.CommArea$After.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.Robbery.After            <- Street.Lights.AllOut.CommArea$Robbery.After.sum           /Street.Lights.AllOut.CommArea$After.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.Assault.After            <- Street.Lights.AllOut.CommArea$Assault.After.sum           /Street.Lights.AllOut.CommArea$After.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.Burglary.After           <- Street.Lights.AllOut.CommArea$Burglary.After.sum          /Street.Lights.AllOut.CommArea$After.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.Homicide.After           <- Street.Lights.AllOut.CommArea$Homicide.After.sum          /Street.Lights.AllOut.CommArea$After.Period.Duration.sum
Street.Lights.AllOut.CommArea$Rate.AllCrimes.BeforeAfter         <- (Street.Lights.AllOut.CommArea$Crimes.All.Before.sum       +Street.Lights.AllOut.CommArea$Crimes.All.After.sum)       /(Street.Lights.AllOut.CommArea$Before.Period.Duration.sum+Street.Lights.AllOut.CommArea$After.Period.Duration.sum)
Street.Lights.AllOut.CommArea$Rate.Thefts.BeforeAfter            <- (Street.Lights.AllOut.CommArea$Thefts.Before.sum           +Street.Lights.AllOut.CommArea$Thefts.After.sum)           /(Street.Lights.AllOut.CommArea$Before.Period.Duration.sum+Street.Lights.AllOut.CommArea$After.Period.Duration.sum)
Street.Lights.AllOut.CommArea$Rate.Narcotics.BeforeAfter         <- (Street.Lights.AllOut.CommArea$Narcotics.Before.sum        +Street.Lights.AllOut.CommArea$Narcotics.After.sum)        /(Street.Lights.AllOut.CommArea$Before.Period.Duration.sum+Street.Lights.AllOut.CommArea$After.Period.Duration.sum)
Street.Lights.AllOut.CommArea$Rate.Battery.BeforeAfter           <- (Street.Lights.AllOut.CommArea$Battery.Before.sum          +Street.Lights.AllOut.CommArea$Battery.After.sum)          /(Street.Lights.AllOut.CommArea$Before.Period.Duration.sum+Street.Lights.AllOut.CommArea$After.Period.Duration.sum)
Street.Lights.AllOut.CommArea$Rate.CriminalDamage.BeforeAfter    <- (Street.Lights.AllOut.CommArea$CriminalDamage.Before.sum   +Street.Lights.AllOut.CommArea$CriminalDamage.After.sum)   /(Street.Lights.AllOut.CommArea$Before.Period.Duration.sum+Street.Lights.AllOut.CommArea$After.Period.Duration.sum)
Street.Lights.AllOut.CommArea$Rate.MotorVehicleTheft.BeforeAfter <- (Street.Lights.AllOut.CommArea$MotorVehicleTheft.Before.sum+Street.Lights.AllOut.CommArea$MotorVehicleTheft.After.sum)/(Street.Lights.AllOut.CommArea$Before.Period.Duration.sum+Street.Lights.AllOut.CommArea$After.Period.Duration.sum)
Street.Lights.AllOut.CommArea$Rate.Robbery.BeforeAfter           <- (Street.Lights.AllOut.CommArea$Robbery.Before.sum          +Street.Lights.AllOut.CommArea$Robbery.After.sum)          /(Street.Lights.AllOut.CommArea$Before.Period.Duration.sum+Street.Lights.AllOut.CommArea$After.Period.Duration.sum)
Street.Lights.AllOut.CommArea$Rate.Assault.BeforeAfter           <- (Street.Lights.AllOut.CommArea$Assault.Before.sum          +Street.Lights.AllOut.CommArea$Assault.After.sum)          /(Street.Lights.AllOut.CommArea$Before.Period.Duration.sum+Street.Lights.AllOut.CommArea$After.Period.Duration.sum)
Street.Lights.AllOut.CommArea$Rate.Burglary.BeforeAfter          <- (Street.Lights.AllOut.CommArea$Burglary.Before.sum         +Street.Lights.AllOut.CommArea$Burglary.After.sum)         /(Street.Lights.AllOut.CommArea$Before.Period.Duration.sum+Street.Lights.AllOut.CommArea$After.Period.Duration.sum)
Street.Lights.AllOut.CommArea$Rate.Homicide.BeforeAfter          <- (Street.Lights.AllOut.CommArea$Homicide.Before.sum         +Street.Lights.AllOut.CommArea$Homicide.After.sum)         /(Street.Lights.AllOut.CommArea$Before.Period.Duration.sum+Street.Lights.AllOut.CommArea$After.Period.Duration.sum)
Street.Lights.AllOut.CommArea$AbsDiff.AllCrimes         <- Street.Lights.AllOut.CommArea$Rate.AllCrimes.During         - Street.Lights.AllOut.CommArea$Rate.AllCrimes.BeforeAfter 
Street.Lights.AllOut.CommArea$AbsDiff.Thefts            <- Street.Lights.AllOut.CommArea$Rate.Thefts.During            - Street.Lights.AllOut.CommArea$Rate.Thefts.BeforeAfter 
Street.Lights.AllOut.CommArea$AbsDiff.Narcotics         <- Street.Lights.AllOut.CommArea$Rate.Narcotics.During         - Street.Lights.AllOut.CommArea$Rate.Narcotics.BeforeAfter 
Street.Lights.AllOut.CommArea$AbsDiff.Battery           <- Street.Lights.AllOut.CommArea$Rate.Battery.During           - Street.Lights.AllOut.CommArea$Rate.Battery.BeforeAfter 
Street.Lights.AllOut.CommArea$AbsDiff.CriminalDamage    <- Street.Lights.AllOut.CommArea$Rate.CriminalDamage.During    - Street.Lights.AllOut.CommArea$Rate.CriminalDamage.BeforeAfter 
Street.Lights.AllOut.CommArea$AbsDiff.MotorVehicleTheft <- Street.Lights.AllOut.CommArea$Rate.MotorVehicleTheft.During - Street.Lights.AllOut.CommArea$Rate.MotorVehicleTheft.BeforeAfter 
Street.Lights.AllOut.CommArea$AbsDiff.Robbery           <- Street.Lights.AllOut.CommArea$Rate.Robbery.During           - Street.Lights.AllOut.CommArea$Rate.Robbery.BeforeAfter 
Street.Lights.AllOut.CommArea$AbsDiff.Assault           <- Street.Lights.AllOut.CommArea$Rate.Assault.During           - Street.Lights.AllOut.CommArea$Rate.Assault.BeforeAfter 
Street.Lights.AllOut.CommArea$AbsDiff.Burglary          <- Street.Lights.AllOut.CommArea$Rate.Burglary.During          - Street.Lights.AllOut.CommArea$Rate.Burglary.BeforeAfter 
Street.Lights.AllOut.CommArea$AbsDiff.Homicide          <- Street.Lights.AllOut.CommArea$Rate.Homicide.During          - Street.Lights.AllOut.CommArea$Rate.Homicide.BeforeAfter 
Street.Lights.AllOut.CommArea$PctDiff.AllCrimes          <- 100*Street.Lights.AllOut.CommArea$AbsDiff.AllCrimes        / Street.Lights.AllOut.CommArea$Rate.AllCrimes.BeforeAfter
Street.Lights.AllOut.CommArea$PctDiff.Thefts             <- 100*Street.Lights.AllOut.CommArea$AbsDiff.Thefts           / Street.Lights.AllOut.CommArea$Rate.Thefts.BeforeAfter
Street.Lights.AllOut.CommArea$PctDiff.Narcotics          <- 100*Street.Lights.AllOut.CommArea$AbsDiff.Narcotics        / Street.Lights.AllOut.CommArea$Rate.Narcotics.BeforeAfter
Street.Lights.AllOut.CommArea$PctDiff.Battery            <- 100*Street.Lights.AllOut.CommArea$AbsDiff.Battery          / Street.Lights.AllOut.CommArea$Rate.Battery.BeforeAfter
Street.Lights.AllOut.CommArea$PctDiff.CriminalDamage     <- 100*Street.Lights.AllOut.CommArea$AbsDiff.CriminalDamage   / Street.Lights.AllOut.CommArea$Rate.CriminalDamage.BeforeAfter
Street.Lights.AllOut.CommArea$PctDiff.MotorVehicleTheft  <- 100*Street.Lights.AllOut.CommArea$AbsDiff.MotorVehicleTheft/ Street.Lights.AllOut.CommArea$Rate.MotorVehicleTheft.BeforeAfter
Street.Lights.AllOut.CommArea$PctDiff.Robbery            <- 100*Street.Lights.AllOut.CommArea$AbsDiff.Robbery          / Street.Lights.AllOut.CommArea$Rate.Robbery.BeforeAfter
Street.Lights.AllOut.CommArea$PctDiff.Assault            <- 100*Street.Lights.AllOut.CommArea$AbsDiff.Assault          / Street.Lights.AllOut.CommArea$Rate.Assault.BeforeAfter
Street.Lights.AllOut.CommArea$PctDiff.Burglary           <- 100*Street.Lights.AllOut.CommArea$AbsDiff.Burglary         / Street.Lights.AllOut.CommArea$Rate.Burglary.BeforeAfter
Street.Lights.AllOut.CommArea$PctDiff.Homicide           <- 100*Street.Lights.AllOut.CommArea$AbsDiff.Homicide         / Street.Lights.AllOut.CommArea$Rate.Homicide.BeforeAfter


#Create Datasets Formatted for Poisson Modeling and Estimation of SE's
Alley.Lights.Pois <- Alley.Lights[rep(seq(nrow(Alley.Lights)),3),]
Alley.Lights.Pois <- Alley.Lights.Pois[order(Alley.Lights.Pois$Service.Request.No),]
Alley.Lights.Pois$Time              <- rep(c("BEFORE", "DURING", "AFTER"), nrow(Alley.Lights))
Alley.Lights.Pois$AllCrimes         <- numeric(nrow(Alley.Lights.Pois))
Alley.Lights.Pois$Thefts            <- numeric(nrow(Alley.Lights.Pois))
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
Alley.Lights.Pois$Thefts[which(Alley.Lights.Pois$Time=="BEFORE")]            <- Alley.Lights.Pois$Thefts.Before[which(Alley.Lights.Pois$Time=="BEFORE")]
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
Alley.Lights.Pois$Thefts[which(Alley.Lights.Pois$Time=="DURING")]            <- Alley.Lights.Pois$Thefts.During[which(Alley.Lights.Pois$Time=="DURING")]
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
Alley.Lights.Pois$Thefts[which(Alley.Lights.Pois$Time=="AFTER")]            <- Alley.Lights.Pois$Thefts.After[which(Alley.Lights.Pois$Time=="AFTER")]
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
                                           "police_district", "community_area", "Time", "AllCrimes", "Thefts", "Narcotics", "Battery", "CriminalDamage", 
                                           "MotorVehicleTheft", "Robbery", "Assault", "Burglary", "Homicide", "DeceptivePractice", "Duration")]
Alley.Lights.Pois$OutageInd <- numeric(nrow(Alley.Lights.Pois))
Alley.Lights.Pois$OutageInd[which(Alley.Lights.Pois$Time=="DURING")] <- rep(1, length(which(Alley.Lights.Pois$Time=="DURING")))

Fit.Alley.AllCrimes         <- glm(AllCrimes         ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Thefts            <- glm(Thefts            ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Alley.Lights.Pois)
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
Summary.Table.Alley$p.value[2]  <- summary(Fit.Alley.Thefts)$coef[2,4]
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
Summary.Table.Alley$LL[2]  <- 100*exp(Fit.Alley.Thefts$coefficients[2]            - qt(.975, length(Fit.Alley.Thefts$residuals-1))           *summary(Fit.Alley.Thefts)$coef[2,2])           -100
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
Summary.Table.Alley$UL[2]  <- 100*exp(Fit.Alley.Thefts$coefficients[2]            + qt(.975, length(Fit.Alley.Thefts$residuals-1))           *summary(Fit.Alley.Thefts)$coef[2,2])           -100
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
Street.Lights.OneOut.Pois$Thefts            <- numeric(nrow(Street.Lights.OneOut.Pois))
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
Street.Lights.OneOut.Pois$Thefts[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]            <- Street.Lights.OneOut.Pois$Thefts.Before[which(Street.Lights.OneOut.Pois$Time=="BEFORE")]
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
Street.Lights.OneOut.Pois$Thefts[which(Street.Lights.OneOut.Pois$Time=="DURING")]            <- Street.Lights.OneOut.Pois$Thefts.During[which(Street.Lights.OneOut.Pois$Time=="DURING")]
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
Street.Lights.OneOut.Pois$Thefts[which(Street.Lights.OneOut.Pois$Time=="AFTER")]            <- Street.Lights.OneOut.Pois$Thefts.After[which(Street.Lights.OneOut.Pois$Time=="AFTER")]
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
                                          "police_district", "community_area", "Time", "AllCrimes", "Thefts", "Narcotics", "Battery", "CriminalDamage", 
                                          "MotorVehicleTheft", "Robbery", "Assault", "Burglary", "Homicide", "DeceptivePractice", "Duration")]
Street.Lights.OneOut.Pois$OutageInd <- numeric(nrow(Street.Lights.OneOut.Pois))
Street.Lights.OneOut.Pois$OutageInd[which(Street.Lights.OneOut.Pois$Time=="DURING")] <- rep(1, length(which(Street.Lights.OneOut.Pois$Time=="DURING")))

Fit.OneOut.AllCrimes         <- glm(AllCrimes         ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Thefts            <- glm(Thefts            ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
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
Summary.Table.OneOut$p.value[2] <- summary(Fit.OneOut.Thefts)$coef[2,4]
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
Summary.Table.OneOut$LL[2]  <- 100*exp(Fit.OneOut.Thefts$coefficients[2]            - qt(.975, length(Fit.OneOut.Thefts$residuals-1))           *summary(Fit.OneOut.Thefts)$coef[2,2])           -100
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
Summary.Table.OneOut$UL[2]  <- 100*exp(Fit.OneOut.Thefts$coefficients[2]            + qt(.975, length(Fit.OneOut.Thefts$residuals-1))           *summary(Fit.OneOut.Thefts)$coef[2,2])           -100
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
Street.Lights.AllOut.Pois$Thefts            <- numeric(nrow(Street.Lights.AllOut.Pois))
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
Street.Lights.AllOut.Pois$Thefts[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]            <- Street.Lights.AllOut.Pois$Thefts.Before[which(Street.Lights.AllOut.Pois$Time=="BEFORE")]
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
Street.Lights.AllOut.Pois$Thefts[which(Street.Lights.AllOut.Pois$Time=="DURING")]            <- Street.Lights.AllOut.Pois$Thefts.During[which(Street.Lights.AllOut.Pois$Time=="DURING")]
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
Street.Lights.AllOut.Pois$Thefts[which(Street.Lights.AllOut.Pois$Time=="AFTER")]            <- Street.Lights.AllOut.Pois$Thefts.After[which(Street.Lights.AllOut.Pois$Time=="AFTER")]
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
                                                          "police_district", "community_area", "Time", "AllCrimes", "Thefts", "Narcotics", "Battery", "CriminalDamage", 
                                                          "MotorVehicleTheft", "Robbery", "Assault", "Burglary", "Homicide", "DeceptivePractice", "Duration")]
Street.Lights.AllOut.Pois$OutageInd <- numeric(nrow(Street.Lights.AllOut.Pois))
Street.Lights.AllOut.Pois$OutageInd[which(Street.Lights.AllOut.Pois$Time=="DURING")] <- rep(1, length(which(Street.Lights.AllOut.Pois$Time=="DURING")))

Fit.AllOut.AllCrimes         <- glm(AllCrimes         ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Thefts            <- glm(Thefts            ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
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
Summary.Table.AllOut$p.value[2] <- summary(Fit.AllOut.Thefts)$coef[2,4]
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
Summary.Table.AllOut$LL[2]  <- 100*exp(Fit.AllOut.Thefts$coefficients[2]            - qt(.975, length(Fit.AllOut.Thefts$residuals-1))           *summary(Fit.AllOut.Thefts)$coef[2,2])           -100
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
Summary.Table.AllOut$UL[2]  <- 100*exp(Fit.AllOut.Thefts$coefficients[2]            + qt(.975, length(Fit.AllOut.Thefts$residuals-1))           *summary(Fit.AllOut.Thefts)$coef[2,2])           -100
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
Alley.Lights.Pois$Month[which(Alley.Lights.Pois$Time=="BEFORE")] <- as.numeric(substr(as.Date(as.numeric(Alley.Lights.Pois$DateCreated)-22                                               , origin = "2012-04-01"),6,7))[which(Alley.Lights.Pois$Time=="BEFORE")] 
Alley.Lights.Pois$Month[which(Alley.Lights.Pois$Time=="DURING")] <- as.numeric(substr(as.Date(as.numeric(Alley.Lights.Pois$DateCreated)+round(Alley.Lights.Pois$Duration/2,0)    , origin = "2012-04-01"),6,7))[which(Alley.Lights.Pois$Time=="DURING")] 
Alley.Lights.Pois$Month[which(Alley.Lights.Pois$Time=="AFTER")]  <- as.numeric(substr(as.Date(as.numeric(Alley.Lights.Pois$DateCompleted)+7+round(Alley.Lights.Pois$Duration/2,0), origin = "2012-04-01"),6,7))[which(Alley.Lights.Pois$Time=="AFTER")] 
Alley.Lights.Pois$Jan <- as.numeric(Alley.Lights.Pois$Month==1)
Alley.Lights.Pois$Feb <- as.numeric(Alley.Lights.Pois$Month==2)
Alley.Lights.Pois$Mar <- as.numeric(Alley.Lights.Pois$Month==3)
Alley.Lights.Pois$Apr <- as.numeric(Alley.Lights.Pois$Month==4)
Alley.Lights.Pois$May <- as.numeric(Alley.Lights.Pois$Month==5)
Alley.Lights.Pois$Jun <- as.numeric(Alley.Lights.Pois$Month==6)
Alley.Lights.Pois$Jul <- as.numeric(Alley.Lights.Pois$Month==7)
Alley.Lights.Pois$Aug <- as.numeric(Alley.Lights.Pois$Month==8)
Alley.Lights.Pois$Sep <- as.numeric(Alley.Lights.Pois$Month==9)
Alley.Lights.Pois$Oct <- as.numeric(Alley.Lights.Pois$Month==10)
Alley.Lights.Pois$Nov <- as.numeric(Alley.Lights.Pois$Month==11)
Alley.Lights.Pois$Dec <- as.numeric(Alley.Lights.Pois$Month==12)

Fit.Alley.AllCrimes.Seas         <- glm(AllCrimes         ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Thefts.Seas            <- glm(Thefts            ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Narcotics.Seas         <- glm(Narcotics         ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Battery.Seas           <- glm(Battery           ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.CriminalDamage.Seas    <- glm(CriminalDamage    ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.MotorVehicleTheft.Seas <- glm(MotorVehicleTheft ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Robbery.Seas           <- glm(Robbery           ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Assault.Seas           <- glm(Assault           ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Burglary.Seas          <- glm(Burglary          ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Homicide.Seas          <- glm(Homicide          ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.DeceptivePractice.Seas <- glm(DeceptivePractice ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Alley.Lights.Pois)
Summary.Table.Alley$PctDiff.Seas <- numeric(nrow(Summary.Table.Alley))
Summary.Table.Alley$LL.Seas <- numeric(nrow(Summary.Table.Alley))
Summary.Table.Alley$UL.Seas <- numeric(nrow(Summary.Table.Alley))
Summary.Table.Alley$p.value.Seas <- numeric(nrow(Summary.Table.Alley))
Summary.Table.Alley$star.Seas <- character(nrow(Summary.Table.Alley))
Summary.Table.Alley$PctDiff.Seas[1] <- 100*exp(Fit.Alley.AllCrimes.Seas$coef[2])-100
Summary.Table.Alley$PctDiff.Seas[2] <- 100*exp(Fit.Alley.Thefts.Seas$coef[2])-100
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
Summary.Table.Alley$p.value.Seas[2] <- summary(Fit.Alley.Thefts.Seas)$coef[2,4]
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
Summary.Table.Alley$LL.Seas[2]  <- 100*exp(Fit.Alley.Thefts.Seas$coefficients[2]            - qt(.975, length(Fit.Alley.Thefts.Seas$residuals-1))           *summary(Fit.Alley.Thefts.Seas)$coef[2,2])           -100
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
Summary.Table.Alley$UL.Seas[2]  <- 100*exp(Fit.Alley.Thefts.Seas$coefficients[2]            + qt(.975, length(Fit.Alley.Thefts.Seas$residuals-1))           *summary(Fit.Alley.Thefts.Seas)$coef[2,2])           -100
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
Street.Lights.OneOut.Pois$Month[which(Street.Lights.OneOut.Pois$Time=="BEFORE")] <- as.numeric(substr(as.Date(as.numeric(Street.Lights.OneOut.Pois$DateCreated)-22                                               , origin = "2012-04-01"),6,7))[which(Street.Lights.OneOut.Pois$Time=="BEFORE")] 
Street.Lights.OneOut.Pois$Month[which(Street.Lights.OneOut.Pois$Time=="DURING")] <- as.numeric(substr(as.Date(as.numeric(Street.Lights.OneOut.Pois$DateCreated)+round(Street.Lights.OneOut.Pois$Duration/2,0)    , origin = "2012-04-01"),6,7))[which(Street.Lights.OneOut.Pois$Time=="DURING")] 
Street.Lights.OneOut.Pois$Month[which(Street.Lights.OneOut.Pois$Time=="AFTER")]  <- as.numeric(substr(as.Date(as.numeric(Street.Lights.OneOut.Pois$DateCompleted)+7+round(Street.Lights.OneOut.Pois$Duration/2,0), origin = "2012-04-01"),6,7))[which(Street.Lights.OneOut.Pois$Time=="AFTER")] 
Street.Lights.OneOut.Pois$Jan <- as.numeric(Street.Lights.OneOut.Pois$Month==1)
Street.Lights.OneOut.Pois$Feb <- as.numeric(Street.Lights.OneOut.Pois$Month==2)
Street.Lights.OneOut.Pois$Mar <- as.numeric(Street.Lights.OneOut.Pois$Month==3)
Street.Lights.OneOut.Pois$Apr <- as.numeric(Street.Lights.OneOut.Pois$Month==4)
Street.Lights.OneOut.Pois$May <- as.numeric(Street.Lights.OneOut.Pois$Month==5)
Street.Lights.OneOut.Pois$Jun <- as.numeric(Street.Lights.OneOut.Pois$Month==6)
Street.Lights.OneOut.Pois$Jul <- as.numeric(Street.Lights.OneOut.Pois$Month==7)
Street.Lights.OneOut.Pois$Aug <- as.numeric(Street.Lights.OneOut.Pois$Month==8)
Street.Lights.OneOut.Pois$Sep <- as.numeric(Street.Lights.OneOut.Pois$Month==9)
Street.Lights.OneOut.Pois$Oct <- as.numeric(Street.Lights.OneOut.Pois$Month==10)
Street.Lights.OneOut.Pois$Nov <- as.numeric(Street.Lights.OneOut.Pois$Month==11)
Street.Lights.OneOut.Pois$Dec <- as.numeric(Street.Lights.OneOut.Pois$Month==12)

Fit.OneOut.AllCrimes.Seas         <- glm(AllCrimes         ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Thefts.Seas            <- glm(Thefts            ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Narcotics.Seas         <- glm(Narcotics         ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Battery.Seas           <- glm(Battery           ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.CriminalDamage.Seas    <- glm(CriminalDamage    ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.MotorVehicleTheft.Seas <- glm(MotorVehicleTheft ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Robbery.Seas           <- glm(Robbery           ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Assault.Seas           <- glm(Assault           ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Burglary.Seas          <- glm(Burglary          ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Homicide.Seas          <- glm(Homicide          ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.DeceptivePractice.Seas <- glm(DeceptivePractice ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Summary.Table.OneOut$PctDiff.Seas <- numeric(nrow(Summary.Table.OneOut))
Summary.Table.OneOut$LL.Seas <- numeric(nrow(Summary.Table.OneOut))
Summary.Table.OneOut$UL.Seas <- numeric(nrow(Summary.Table.OneOut))
Summary.Table.OneOut$p.value.Seas <- numeric(nrow(Summary.Table.OneOut))
Summary.Table.OneOut$star.Seas <- character(nrow(Summary.Table.OneOut))
Summary.Table.OneOut$PctDiff.Seas[1] <- 100*exp(Fit.OneOut.AllCrimes.Seas$coef[2])-100
Summary.Table.OneOut$PctDiff.Seas[2] <- 100*exp(Fit.OneOut.Thefts.Seas$coef[2])-100
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
Summary.Table.OneOut$p.value.Seas[2] <- summary(Fit.OneOut.Thefts.Seas)$coef[2,4]
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
Summary.Table.OneOut$LL.Seas[2]  <- 100*exp(Fit.OneOut.Thefts.Seas$coefficients[2]            - qt(.975, length(Fit.OneOut.Thefts.Seas$residuals-1))           *summary(Fit.OneOut.Thefts.Seas)$coef[2,2])           -100
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
Summary.Table.OneOut$UL.Seas[2]  <- 100*exp(Fit.OneOut.Thefts.Seas$coefficients[2]            + qt(.975, length(Fit.OneOut.Thefts.Seas$residuals-1))           *summary(Fit.OneOut.Thefts.Seas)$coef[2,2])           -100
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
Street.Lights.AllOut.Pois$Month[which(Street.Lights.AllOut.Pois$Time=="BEFORE")] <- as.numeric(substr(as.Date(as.numeric(Street.Lights.AllOut.Pois$DateCreated)-22                                               , origin = "2012-04-01"),6,7))[which(Street.Lights.AllOut.Pois$Time=="BEFORE")] 
Street.Lights.AllOut.Pois$Month[which(Street.Lights.AllOut.Pois$Time=="DURING")] <- as.numeric(substr(as.Date(as.numeric(Street.Lights.AllOut.Pois$DateCreated)+round(Street.Lights.AllOut.Pois$Duration/2,0)    , origin = "2012-04-01"),6,7))[which(Street.Lights.AllOut.Pois$Time=="DURING")] 
Street.Lights.AllOut.Pois$Month[which(Street.Lights.AllOut.Pois$Time=="AFTER")]  <- as.numeric(substr(as.Date(as.numeric(Street.Lights.AllOut.Pois$DateCompleted)+7+round(Street.Lights.AllOut.Pois$Duration/2,0), origin = "2012-04-01"),6,7))[which(Street.Lights.AllOut.Pois$Time=="AFTER")] 
Street.Lights.AllOut.Pois$Jan <- as.numeric(Street.Lights.AllOut.Pois$Month==1)
Street.Lights.AllOut.Pois$Feb <- as.numeric(Street.Lights.AllOut.Pois$Month==2)
Street.Lights.AllOut.Pois$Mar <- as.numeric(Street.Lights.AllOut.Pois$Month==3)
Street.Lights.AllOut.Pois$Apr <- as.numeric(Street.Lights.AllOut.Pois$Month==4)
Street.Lights.AllOut.Pois$May <- as.numeric(Street.Lights.AllOut.Pois$Month==5)
Street.Lights.AllOut.Pois$Jun <- as.numeric(Street.Lights.AllOut.Pois$Month==6)
Street.Lights.AllOut.Pois$Jul <- as.numeric(Street.Lights.AllOut.Pois$Month==7)
Street.Lights.AllOut.Pois$Aug <- as.numeric(Street.Lights.AllOut.Pois$Month==8)
Street.Lights.AllOut.Pois$Sep <- as.numeric(Street.Lights.AllOut.Pois$Month==9)
Street.Lights.AllOut.Pois$Oct <- as.numeric(Street.Lights.AllOut.Pois$Month==10)
Street.Lights.AllOut.Pois$Nov <- as.numeric(Street.Lights.AllOut.Pois$Month==11)
Street.Lights.AllOut.Pois$Dec <- as.numeric(Street.Lights.AllOut.Pois$Month==12)

Fit.AllOut.AllCrimes.Seas         <- glm(AllCrimes         ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Thefts.Seas            <- glm(Thefts            ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Narcotics.Seas         <- glm(Narcotics         ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Battery.Seas           <- glm(Battery           ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.CriminalDamage.Seas    <- glm(CriminalDamage    ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.MotorVehicleTheft.Seas <- glm(MotorVehicleTheft ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Robbery.Seas           <- glm(Robbery           ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Assault.Seas           <- glm(Assault           ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Burglary.Seas          <- glm(Burglary          ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Homicide.Seas          <- glm(Homicide          ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.DeceptivePractice.Seas <- glm(DeceptivePractice ~ offset(log(Duration)) + OutageInd + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Summary.Table.AllOut$PctDiff.Seas <- numeric(nrow(Summary.Table.AllOut))
Summary.Table.AllOut$LL.Seas <- numeric(nrow(Summary.Table.AllOut))
Summary.Table.AllOut$UL.Seas <- numeric(nrow(Summary.Table.AllOut))
Summary.Table.AllOut$p.value.Seas <- numeric(nrow(Summary.Table.AllOut))
Summary.Table.AllOut$star.Seas <- character(nrow(Summary.Table.AllOut))
Summary.Table.AllOut$PctDiff.Seas[1] <- 100*exp(Fit.AllOut.AllCrimes.Seas$coef[2])-100
Summary.Table.AllOut$PctDiff.Seas[2] <- 100*exp(Fit.AllOut.Thefts.Seas$coef[2])-100
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
Summary.Table.AllOut$p.value.Seas[2] <- summary(Fit.AllOut.Thefts.Seas)$coef[2,4]
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
Summary.Table.AllOut$LL.Seas[2]  <- 100*exp(Fit.AllOut.Thefts.Seas$coefficients[2]            - qt(.975, length(Fit.AllOut.Thefts.Seas$residuals-1))           *summary(Fit.AllOut.Thefts.Seas)$coef[2,2])           -100
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
Summary.Table.AllOut$UL.Seas[2]  <- 100*exp(Fit.AllOut.Thefts.Seas$coefficients[2]            + qt(.975, length(Fit.AllOut.Thefts.Seas$residuals-1))           *summary(Fit.AllOut.Thefts.Seas)$coef[2,2])           -100
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




## Monthly Effect Specifications
Alley.Lights.Pois$JanInd <- Alley.Lights.Pois$Jan * Alley.Lights.Pois$OutageInd
Alley.Lights.Pois$FebInd <- Alley.Lights.Pois$Feb * Alley.Lights.Pois$OutageInd
Alley.Lights.Pois$MarInd <- Alley.Lights.Pois$Mar * Alley.Lights.Pois$OutageInd
Alley.Lights.Pois$AprInd <- Alley.Lights.Pois$Apr * Alley.Lights.Pois$OutageInd
Alley.Lights.Pois$MayInd <- Alley.Lights.Pois$May * Alley.Lights.Pois$OutageInd
Alley.Lights.Pois$JunInd <- Alley.Lights.Pois$Jun * Alley.Lights.Pois$OutageInd
Alley.Lights.Pois$JulInd <- Alley.Lights.Pois$Jul * Alley.Lights.Pois$OutageInd
Alley.Lights.Pois$AugInd <- Alley.Lights.Pois$Aug * Alley.Lights.Pois$OutageInd
Alley.Lights.Pois$SepInd <- Alley.Lights.Pois$Sep * Alley.Lights.Pois$OutageInd
Alley.Lights.Pois$OctInd <- Alley.Lights.Pois$Oct * Alley.Lights.Pois$OutageInd
Alley.Lights.Pois$NovInd <- Alley.Lights.Pois$Nov * Alley.Lights.Pois$OutageInd
Alley.Lights.Pois$DecInd <- Alley.Lights.Pois$Dec * Alley.Lights.Pois$OutageInd

Summary.Table.Alley.Mon  <- data.frame(matrix(ncol = 36, nrow = 11), row.names = c("All Crimes (No Deceptive Practice)", 
                                                                              "Thefts", "Narcotics", "Battery", "Criminal Damage", "Motor Vehicle Theft", 
                                                                              "Robbery", "Assault", "Burglary", "Homicide", "Deceptive Practice"))

Summary.Table.Alley.Mon  <- rename(Summary.Table.Alley.Mon,      c("X1"="JanEffect",  "X2"="JanPValue",  "X3"="JanStar", 
                                                               "X4"="FebEffect",  "X5"="FebPValue",  "X6"="FebStar",
                                                               "X7"="MarEffect",  "X8"="MarPValue",  "X9"="MarStar",
                                                               "X10"="AprEffect", "X11"="AprPValue", "X12"="AprStar",
                                                               "X13"="MayEffect", "X14"="MayPValue", "X15"="MayStar",
                                                               "X16"="JunEffect", "X17"="JunPValue", "X18"="JunStar",
                                                               "X19"="JulEffect", "X20"="JulPValue", "X21"="JulStar",
                                                               "X22"="AugEffect", "X23"="AugPValue", "X24"="AugStar",
                                                               "X25"="SepEffect", "X26"="SepPValue", "X27"="SepStar",
                                                               "X28"="OctEffect", "X29"="OctPValue", "X30"="OctStar",
                                                               "X31"="NovEffect", "X32"="NovPValue", "X33"="NovStar",
                                                               "X34"="DecEffect", "X35"="DecPValue", "X36"="DecStar"))



Fit.Alley.AllCrimes.Mon         <- glm(AllCrimes         ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Thefts.Mon            <- glm(Thefts            ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Narcotics.Mon         <- glm(Narcotics         ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Battery.Mon           <- glm(Battery           ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.CriminalDamage.Mon    <- glm(CriminalDamage    ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.MotorVehicleTheft.Mon <- glm(MotorVehicleTheft ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Robbery.Mon           <- glm(Robbery           ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Assault.Mon           <- glm(Assault           ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Burglary.Mon          <- glm(Burglary          ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.Homicide.Mon          <- glm(Homicide          ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Alley.Lights.Pois)
Fit.Alley.DeceptivePractice.Mon <- glm(DeceptivePractice ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Alley.Lights.Pois)

Summary.Table.Alley.Mon$JanStar <- character(nrow(Summary.Table.Alley.Mon))
Summary.Table.Alley.Mon$FebStar <- character(nrow(Summary.Table.Alley.Mon))
Summary.Table.Alley.Mon$MarStar <- character(nrow(Summary.Table.Alley.Mon))
Summary.Table.Alley.Mon$AprStar <- character(nrow(Summary.Table.Alley.Mon))
Summary.Table.Alley.Mon$MayStar <- character(nrow(Summary.Table.Alley.Mon))
Summary.Table.Alley.Mon$JunStar <- character(nrow(Summary.Table.Alley.Mon))
Summary.Table.Alley.Mon$JulStar <- character(nrow(Summary.Table.Alley.Mon))
Summary.Table.Alley.Mon$AugStar <- character(nrow(Summary.Table.Alley.Mon))
Summary.Table.Alley.Mon$SepStar <- character(nrow(Summary.Table.Alley.Mon))
Summary.Table.Alley.Mon$OctStar <- character(nrow(Summary.Table.Alley.Mon))
Summary.Table.Alley.Mon$NovStar <- character(nrow(Summary.Table.Alley.Mon))
Summary.Table.Alley.Mon$DecStar <- character(nrow(Summary.Table.Alley.Mon))

Summary.Table.Alley.Mon$JanEffect[1] <- 100*exp(Fit.Alley.AllCrimes.Mon$coef[13])-100
Summary.Table.Alley.Mon$JanEffect[2] <- 100*exp(Fit.Alley.Thefts.Mon$coef[13])-100
Summary.Table.Alley.Mon$JanEffect[3] <- 100*exp(Fit.Alley.Narcotics.Mon$coef[13])-100
Summary.Table.Alley.Mon$JanEffect[4] <- 100*exp(Fit.Alley.Battery.Mon$coef[13])-100
Summary.Table.Alley.Mon$JanEffect[5] <- 100*exp(Fit.Alley.CriminalDamage.Mon$coef[13])-100
Summary.Table.Alley.Mon$JanEffect[6] <- 100*exp(Fit.Alley.MotorVehicleTheft.Mon$coef[13])-100
Summary.Table.Alley.Mon$JanEffect[7] <- 100*exp(Fit.Alley.Robbery.Mon$coef[13])-100
Summary.Table.Alley.Mon$JanEffect[8] <- 100*exp(Fit.Alley.Assault.Mon$coef[13])-100
Summary.Table.Alley.Mon$JanEffect[9] <- 100*exp(Fit.Alley.Burglary.Mon$coef[13])-100
Summary.Table.Alley.Mon$JanEffect[10] <- 100*exp(Fit.Alley.Homicide.Mon$coef[13])-100
Summary.Table.Alley.Mon$JanEffect[11] <- 100*exp(Fit.Alley.DeceptivePractice.Mon$coef[13])-100

Summary.Table.Alley.Mon$FebEffect[1] <- 100*exp(Fit.Alley.AllCrimes.Mon$coef[14])-100
Summary.Table.Alley.Mon$FebEffect[2] <- 100*exp(Fit.Alley.Thefts.Mon$coef[14])-100
Summary.Table.Alley.Mon$FebEffect[3] <- 100*exp(Fit.Alley.Narcotics.Mon$coef[14])-100
Summary.Table.Alley.Mon$FebEffect[4] <- 100*exp(Fit.Alley.Battery.Mon$coef[14])-100
Summary.Table.Alley.Mon$FebEffect[5] <- 100*exp(Fit.Alley.CriminalDamage.Mon$coef[14])-100
Summary.Table.Alley.Mon$FebEffect[6] <- 100*exp(Fit.Alley.MotorVehicleTheft.Mon$coef[14])-100
Summary.Table.Alley.Mon$FebEffect[7] <- 100*exp(Fit.Alley.Robbery.Mon$coef[14])-100
Summary.Table.Alley.Mon$FebEffect[8] <- 100*exp(Fit.Alley.Assault.Mon$coef[14])-100
Summary.Table.Alley.Mon$FebEffect[9] <- 100*exp(Fit.Alley.Burglary.Mon$coef[14])-100
Summary.Table.Alley.Mon$FebEffect[10] <- 100*exp(Fit.Alley.Homicide.Mon$coef[14])-100
Summary.Table.Alley.Mon$FebEffect[11] <- 100*exp(Fit.Alley.DeceptivePractice.Mon$coef[14])-100

Summary.Table.Alley.Mon$MarEffect[1] <- 100*exp(Fit.Alley.AllCrimes.Mon$coef[15])-100
Summary.Table.Alley.Mon$MarEffect[2] <- 100*exp(Fit.Alley.Thefts.Mon$coef[15])-100
Summary.Table.Alley.Mon$MarEffect[3] <- 100*exp(Fit.Alley.Narcotics.Mon$coef[15])-100
Summary.Table.Alley.Mon$MarEffect[4] <- 100*exp(Fit.Alley.Battery.Mon$coef[15])-100
Summary.Table.Alley.Mon$MarEffect[5] <- 100*exp(Fit.Alley.CriminalDamage.Mon$coef[15])-100
Summary.Table.Alley.Mon$MarEffect[6] <- 100*exp(Fit.Alley.MotorVehicleTheft.Mon$coef[15])-100
Summary.Table.Alley.Mon$MarEffect[7] <- 100*exp(Fit.Alley.Robbery.Mon$coef[15])-100
Summary.Table.Alley.Mon$MarEffect[8] <- 100*exp(Fit.Alley.Assault.Mon$coef[15])-100
Summary.Table.Alley.Mon$MarEffect[9] <- 100*exp(Fit.Alley.Burglary.Mon$coef[15])-100
Summary.Table.Alley.Mon$MarEffect[10] <- 100*exp(Fit.Alley.Homicide.Mon$coef[15])-100
Summary.Table.Alley.Mon$MarEffect[11] <- 100*exp(Fit.Alley.DeceptivePractice.Mon$coef[15])-100

Summary.Table.Alley.Mon$AprEffect[1] <- 100*exp(Fit.Alley.AllCrimes.Mon$coef[16])-100
Summary.Table.Alley.Mon$AprEffect[2] <- 100*exp(Fit.Alley.Thefts.Mon$coef[16])-100
Summary.Table.Alley.Mon$AprEffect[3] <- 100*exp(Fit.Alley.Narcotics.Mon$coef[16])-100
Summary.Table.Alley.Mon$AprEffect[4] <- 100*exp(Fit.Alley.Battery.Mon$coef[16])-100
Summary.Table.Alley.Mon$AprEffect[5] <- 100*exp(Fit.Alley.CriminalDamage.Mon$coef[16])-100
Summary.Table.Alley.Mon$AprEffect[6] <- 100*exp(Fit.Alley.MotorVehicleTheft.Mon$coef[16])-100
Summary.Table.Alley.Mon$AprEffect[7] <- 100*exp(Fit.Alley.Robbery.Mon$coef[16])-100
Summary.Table.Alley.Mon$AprEffect[8] <- 100*exp(Fit.Alley.Assault.Mon$coef[16])-100
Summary.Table.Alley.Mon$AprEffect[9] <- 100*exp(Fit.Alley.Burglary.Mon$coef[16])-100
Summary.Table.Alley.Mon$AprEffect[10] <- 100*exp(Fit.Alley.Homicide.Mon$coef[16])-100
Summary.Table.Alley.Mon$AprEffect[11] <- 100*exp(Fit.Alley.DeceptivePractice.Mon$coef[16])-100

Summary.Table.Alley.Mon$MayEffect[1] <- 100*exp(Fit.Alley.AllCrimes.Mon$coef[17])-100
Summary.Table.Alley.Mon$MayEffect[2] <- 100*exp(Fit.Alley.Thefts.Mon$coef[17])-100
Summary.Table.Alley.Mon$MayEffect[3] <- 100*exp(Fit.Alley.Narcotics.Mon$coef[17])-100
Summary.Table.Alley.Mon$MayEffect[4] <- 100*exp(Fit.Alley.Battery.Mon$coef[17])-100
Summary.Table.Alley.Mon$MayEffect[5] <- 100*exp(Fit.Alley.CriminalDamage.Mon$coef[17])-100
Summary.Table.Alley.Mon$MayEffect[6] <- 100*exp(Fit.Alley.MotorVehicleTheft.Mon$coef[17])-100
Summary.Table.Alley.Mon$MayEffect[7] <- 100*exp(Fit.Alley.Robbery.Mon$coef[17])-100
Summary.Table.Alley.Mon$MayEffect[8] <- 100*exp(Fit.Alley.Assault.Mon$coef[17])-100
Summary.Table.Alley.Mon$MayEffect[9] <- 100*exp(Fit.Alley.Burglary.Mon$coef[17])-100
Summary.Table.Alley.Mon$MayEffect[10] <- 100*exp(Fit.Alley.Homicide.Mon$coef[17])-100
Summary.Table.Alley.Mon$MayEffect[11] <- 100*exp(Fit.Alley.DeceptivePractice.Mon$coef[17])-100

Summary.Table.Alley.Mon$JunEffect[1] <- 100*exp(Fit.Alley.AllCrimes.Mon$coef[18])-100
Summary.Table.Alley.Mon$JunEffect[2] <- 100*exp(Fit.Alley.Thefts.Mon$coef[18])-100
Summary.Table.Alley.Mon$JunEffect[3] <- 100*exp(Fit.Alley.Narcotics.Mon$coef[18])-100
Summary.Table.Alley.Mon$JunEffect[4] <- 100*exp(Fit.Alley.Battery.Mon$coef[18])-100
Summary.Table.Alley.Mon$JunEffect[5] <- 100*exp(Fit.Alley.CriminalDamage.Mon$coef[18])-100
Summary.Table.Alley.Mon$JunEffect[6] <- 100*exp(Fit.Alley.MotorVehicleTheft.Mon$coef[18])-100
Summary.Table.Alley.Mon$JunEffect[7] <- 100*exp(Fit.Alley.Robbery.Mon$coef[18])-100
Summary.Table.Alley.Mon$JunEffect[8] <- 100*exp(Fit.Alley.Assault.Mon$coef[18])-100
Summary.Table.Alley.Mon$JunEffect[9] <- 100*exp(Fit.Alley.Burglary.Mon$coef[18])-100
Summary.Table.Alley.Mon$JunEffect[10] <- 100*exp(Fit.Alley.Homicide.Mon$coef[18])-100
Summary.Table.Alley.Mon$JunEffect[11] <- 100*exp(Fit.Alley.DeceptivePractice.Mon$coef[18])-100

Summary.Table.Alley.Mon$JulEffect[1] <- 100*exp(Fit.Alley.AllCrimes.Mon$coef[19])-100
Summary.Table.Alley.Mon$JulEffect[2] <- 100*exp(Fit.Alley.Thefts.Mon$coef[19])-100
Summary.Table.Alley.Mon$JulEffect[3] <- 100*exp(Fit.Alley.Narcotics.Mon$coef[19])-100
Summary.Table.Alley.Mon$JulEffect[4] <- 100*exp(Fit.Alley.Battery.Mon$coef[19])-100
Summary.Table.Alley.Mon$JulEffect[5] <- 100*exp(Fit.Alley.CriminalDamage.Mon$coef[19])-100
Summary.Table.Alley.Mon$JulEffect[6] <- 100*exp(Fit.Alley.MotorVehicleTheft.Mon$coef[19])-100
Summary.Table.Alley.Mon$JulEffect[7] <- 100*exp(Fit.Alley.Robbery.Mon$coef[19])-100
Summary.Table.Alley.Mon$JulEffect[8] <- 100*exp(Fit.Alley.Assault.Mon$coef[19])-100
Summary.Table.Alley.Mon$JulEffect[9] <- 100*exp(Fit.Alley.Burglary.Mon$coef[19])-100
Summary.Table.Alley.Mon$JulEffect[10] <- 100*exp(Fit.Alley.Homicide.Mon$coef[19])-100
Summary.Table.Alley.Mon$JulEffect[11] <- 100*exp(Fit.Alley.DeceptivePractice.Mon$coef[19])-100

Summary.Table.Alley.Mon$AugEffect[1] <- 100*exp(Fit.Alley.AllCrimes.Mon$coef[20])-100
Summary.Table.Alley.Mon$AugEffect[2] <- 100*exp(Fit.Alley.Thefts.Mon$coef[20])-100
Summary.Table.Alley.Mon$AugEffect[3] <- 100*exp(Fit.Alley.Narcotics.Mon$coef[20])-100
Summary.Table.Alley.Mon$AugEffect[4] <- 100*exp(Fit.Alley.Battery.Mon$coef[20])-100
Summary.Table.Alley.Mon$AugEffect[5] <- 100*exp(Fit.Alley.CriminalDamage.Mon$coef[20])-100
Summary.Table.Alley.Mon$AugEffect[6] <- 100*exp(Fit.Alley.MotorVehicleTheft.Mon$coef[20])-100
Summary.Table.Alley.Mon$AugEffect[7] <- 100*exp(Fit.Alley.Robbery.Mon$coef[20])-100
Summary.Table.Alley.Mon$AugEffect[8] <- 100*exp(Fit.Alley.Assault.Mon$coef[20])-100
Summary.Table.Alley.Mon$AugEffect[9] <- 100*exp(Fit.Alley.Burglary.Mon$coef[20])-100
Summary.Table.Alley.Mon$AugEffect[10] <- 100*exp(Fit.Alley.Homicide.Mon$coef[20])-100
Summary.Table.Alley.Mon$AugEffect[11] <- 100*exp(Fit.Alley.DeceptivePractice.Mon$coef[20])-100

Summary.Table.Alley.Mon$SepEffect[1] <- 100*exp(Fit.Alley.AllCrimes.Mon$coef[21])-100
Summary.Table.Alley.Mon$SepEffect[2] <- 100*exp(Fit.Alley.Thefts.Mon$coef[21])-100
Summary.Table.Alley.Mon$SepEffect[3] <- 100*exp(Fit.Alley.Narcotics.Mon$coef[21])-100
Summary.Table.Alley.Mon$SepEffect[4] <- 100*exp(Fit.Alley.Battery.Mon$coef[21])-100
Summary.Table.Alley.Mon$SepEffect[5] <- 100*exp(Fit.Alley.CriminalDamage.Mon$coef[21])-100
Summary.Table.Alley.Mon$SepEffect[6] <- 100*exp(Fit.Alley.MotorVehicleTheft.Mon$coef[21])-100
Summary.Table.Alley.Mon$SepEffect[7] <- 100*exp(Fit.Alley.Robbery.Mon$coef[21])-100
Summary.Table.Alley.Mon$SepEffect[8] <- 100*exp(Fit.Alley.Assault.Mon$coef[21])-100
Summary.Table.Alley.Mon$SepEffect[9] <- 100*exp(Fit.Alley.Burglary.Mon$coef[21])-100
Summary.Table.Alley.Mon$SepEffect[10] <- 100*exp(Fit.Alley.Homicide.Mon$coef[21])-100
Summary.Table.Alley.Mon$SepEffect[11] <- 100*exp(Fit.Alley.DeceptivePractice.Mon$coef[21])-100

Summary.Table.Alley.Mon$OctEffect[1] <- 100*exp(Fit.Alley.AllCrimes.Mon$coef[22])-100
Summary.Table.Alley.Mon$OctEffect[2] <- 100*exp(Fit.Alley.Thefts.Mon$coef[22])-100
Summary.Table.Alley.Mon$OctEffect[3] <- 100*exp(Fit.Alley.Narcotics.Mon$coef[22])-100
Summary.Table.Alley.Mon$OctEffect[4] <- 100*exp(Fit.Alley.Battery.Mon$coef[22])-100
Summary.Table.Alley.Mon$OctEffect[5] <- 100*exp(Fit.Alley.CriminalDamage.Mon$coef[22])-100
Summary.Table.Alley.Mon$OctEffect[6] <- 100*exp(Fit.Alley.MotorVehicleTheft.Mon$coef[22])-100
Summary.Table.Alley.Mon$OctEffect[7] <- 100*exp(Fit.Alley.Robbery.Mon$coef[22])-100
Summary.Table.Alley.Mon$OctEffect[8] <- 100*exp(Fit.Alley.Assault.Mon$coef[22])-100
Summary.Table.Alley.Mon$OctEffect[9] <- 100*exp(Fit.Alley.Burglary.Mon$coef[22])-100
Summary.Table.Alley.Mon$OctEffect[10] <- 100*exp(Fit.Alley.Homicide.Mon$coef[22])-100
Summary.Table.Alley.Mon$OctEffect[11] <- 100*exp(Fit.Alley.DeceptivePractice.Mon$coef[22])-100

Summary.Table.Alley.Mon$NovEffect[1] <- 100*exp(Fit.Alley.AllCrimes.Mon$coef[23])-100
Summary.Table.Alley.Mon$NovEffect[2] <- 100*exp(Fit.Alley.Thefts.Mon$coef[23])-100
Summary.Table.Alley.Mon$NovEffect[3] <- 100*exp(Fit.Alley.Narcotics.Mon$coef[23])-100
Summary.Table.Alley.Mon$NovEffect[4] <- 100*exp(Fit.Alley.Battery.Mon$coef[23])-100
Summary.Table.Alley.Mon$NovEffect[5] <- 100*exp(Fit.Alley.CriminalDamage.Mon$coef[23])-100
Summary.Table.Alley.Mon$NovEffect[6] <- 100*exp(Fit.Alley.MotorVehicleTheft.Mon$coef[23])-100
Summary.Table.Alley.Mon$NovEffect[7] <- 100*exp(Fit.Alley.Robbery.Mon$coef[23])-100
Summary.Table.Alley.Mon$NovEffect[8] <- 100*exp(Fit.Alley.Assault.Mon$coef[23])-100
Summary.Table.Alley.Mon$NovEffect[9] <- 100*exp(Fit.Alley.Burglary.Mon$coef[23])-100
Summary.Table.Alley.Mon$NovEffect[10] <- 100*exp(Fit.Alley.Homicide.Mon$coef[23])-100
Summary.Table.Alley.Mon$NovEffect[11] <- 100*exp(Fit.Alley.DeceptivePractice.Mon$coef[23])-100

Summary.Table.Alley.Mon$DecEffect[1] <- 100*exp(Fit.Alley.AllCrimes.Mon$coef[24])-100
Summary.Table.Alley.Mon$DecEffect[2] <- 100*exp(Fit.Alley.Thefts.Mon$coef[24])-100
Summary.Table.Alley.Mon$DecEffect[3] <- 100*exp(Fit.Alley.Narcotics.Mon$coef[24])-100
Summary.Table.Alley.Mon$DecEffect[4] <- 100*exp(Fit.Alley.Battery.Mon$coef[24])-100
Summary.Table.Alley.Mon$DecEffect[5] <- 100*exp(Fit.Alley.CriminalDamage.Mon$coef[24])-100
Summary.Table.Alley.Mon$DecEffect[6] <- 100*exp(Fit.Alley.MotorVehicleTheft.Mon$coef[24])-100
Summary.Table.Alley.Mon$DecEffect[7] <- 100*exp(Fit.Alley.Robbery.Mon$coef[24])-100
Summary.Table.Alley.Mon$DecEffect[8] <- 100*exp(Fit.Alley.Assault.Mon$coef[24])-100
Summary.Table.Alley.Mon$DecEffect[9] <- 100*exp(Fit.Alley.Burglary.Mon$coef[24])-100
Summary.Table.Alley.Mon$DecEffect[10] <- 100*exp(Fit.Alley.Homicide.Mon$coef[24])-100
Summary.Table.Alley.Mon$DecEffect[11] <- 100*exp(Fit.Alley.DeceptivePractice.Mon$coef[24])-100


Summary.Table.Alley.Mon$JanPValue[1] <- summary(Fit.Alley.AllCrimes.Mon)$coef[13,4]
Summary.Table.Alley.Mon$JanPValue[2] <- summary(Fit.Alley.Thefts.Mon)$coef[13,4]
Summary.Table.Alley.Mon$JanPValue[3] <- summary(Fit.Alley.Narcotics.Mon)$coef[13,4]
Summary.Table.Alley.Mon$JanPValue[4] <- summary(Fit.Alley.Battery.Mon)$coef[13,4]
Summary.Table.Alley.Mon$JanPValue[5] <- summary(Fit.Alley.CriminalDamage.Mon)$coef[13,4]
Summary.Table.Alley.Mon$JanPValue[6] <- summary(Fit.Alley.MotorVehicleTheft.Mon)$coef[13,4]
Summary.Table.Alley.Mon$JanPValue[7] <- summary(Fit.Alley.Robbery.Mon)$coef[13,4]
Summary.Table.Alley.Mon$JanPValue[8] <- summary(Fit.Alley.Assault.Mon)$coef[13,4]
Summary.Table.Alley.Mon$JanPValue[9] <- summary(Fit.Alley.Burglary.Mon)$coef[13,4]
Summary.Table.Alley.Mon$JanPValue[10] <- summary(Fit.Alley.Homicide.Mon)$coef[13,4]
Summary.Table.Alley.Mon$JanPValue[11] <- summary(Fit.Alley.DeceptivePractice.Mon)$coef[13,4]

Summary.Table.Alley.Mon$FebPValue[1] <- summary(Fit.Alley.AllCrimes.Mon)$coef[14,4]
Summary.Table.Alley.Mon$FebPValue[2] <- summary(Fit.Alley.Thefts.Mon)$coef[14,4]
Summary.Table.Alley.Mon$FebPValue[3] <- summary(Fit.Alley.Narcotics.Mon)$coef[14,4]
Summary.Table.Alley.Mon$FebPValue[4] <- summary(Fit.Alley.Battery.Mon)$coef[14,4]
Summary.Table.Alley.Mon$FebPValue[5] <- summary(Fit.Alley.CriminalDamage.Mon)$coef[14,4]
Summary.Table.Alley.Mon$FebPValue[6] <- summary(Fit.Alley.MotorVehicleTheft.Mon)$coef[14,4]
Summary.Table.Alley.Mon$FebPValue[7] <- summary(Fit.Alley.Robbery.Mon)$coef[14,4]
Summary.Table.Alley.Mon$FebPValue[8] <- summary(Fit.Alley.Assault.Mon)$coef[14,4]
Summary.Table.Alley.Mon$FebPValue[9] <- summary(Fit.Alley.Burglary.Mon)$coef[14,4]
Summary.Table.Alley.Mon$FebPValue[10] <- summary(Fit.Alley.Homicide.Mon)$coef[14,4]
Summary.Table.Alley.Mon$FebPValue[11] <- summary(Fit.Alley.DeceptivePractice.Mon)$coef[14,4]

Summary.Table.Alley.Mon$MarPValue[1] <- summary(Fit.Alley.AllCrimes.Mon)$coef[15,4]
Summary.Table.Alley.Mon$MarPValue[2] <- summary(Fit.Alley.Thefts.Mon)$coef[15,4]
Summary.Table.Alley.Mon$MarPValue[3] <- summary(Fit.Alley.Narcotics.Mon)$coef[15,4]
Summary.Table.Alley.Mon$MarPValue[4] <- summary(Fit.Alley.Battery.Mon)$coef[15,4]
Summary.Table.Alley.Mon$MarPValue[5] <- summary(Fit.Alley.CriminalDamage.Mon)$coef[15,4]
Summary.Table.Alley.Mon$MarPValue[6] <- summary(Fit.Alley.MotorVehicleTheft.Mon)$coef[15,4]
Summary.Table.Alley.Mon$MarPValue[7] <- summary(Fit.Alley.Robbery.Mon)$coef[15,4]
Summary.Table.Alley.Mon$MarPValue[8] <- summary(Fit.Alley.Assault.Mon)$coef[15,4]
Summary.Table.Alley.Mon$MarPValue[9] <- summary(Fit.Alley.Burglary.Mon)$coef[15,4]
Summary.Table.Alley.Mon$MarPValue[10] <- summary(Fit.Alley.Homicide.Mon)$coef[15,4]
Summary.Table.Alley.Mon$MarPValue[11] <- summary(Fit.Alley.DeceptivePractice.Mon)$coef[15,4]

Summary.Table.Alley.Mon$AprPValue[1] <- summary(Fit.Alley.AllCrimes.Mon)$coef[16,4]
Summary.Table.Alley.Mon$AprPValue[2] <- summary(Fit.Alley.Thefts.Mon)$coef[16,4]
Summary.Table.Alley.Mon$AprPValue[3] <- summary(Fit.Alley.Narcotics.Mon)$coef[16,4]
Summary.Table.Alley.Mon$AprPValue[4] <- summary(Fit.Alley.Battery.Mon)$coef[16,4]
Summary.Table.Alley.Mon$AprPValue[5] <- summary(Fit.Alley.CriminalDamage.Mon)$coef[16,4]
Summary.Table.Alley.Mon$AprPValue[6] <- summary(Fit.Alley.MotorVehicleTheft.Mon)$coef[16,4]
Summary.Table.Alley.Mon$AprPValue[7] <- summary(Fit.Alley.Robbery.Mon)$coef[16,4]
Summary.Table.Alley.Mon$AprPValue[8] <- summary(Fit.Alley.Assault.Mon)$coef[16,4]
Summary.Table.Alley.Mon$AprPValue[9] <- summary(Fit.Alley.Burglary.Mon)$coef[16,4]
Summary.Table.Alley.Mon$AprPValue[10] <- summary(Fit.Alley.Homicide.Mon)$coef[16,4]
Summary.Table.Alley.Mon$AprPValue[11] <- summary(Fit.Alley.DeceptivePractice.Mon)$coef[16,4]

Summary.Table.Alley.Mon$MayPValue[1] <- summary(Fit.Alley.AllCrimes.Mon)$coef[17,4]
Summary.Table.Alley.Mon$MayPValue[2] <- summary(Fit.Alley.Thefts.Mon)$coef[17,4]
Summary.Table.Alley.Mon$MayPValue[3] <- summary(Fit.Alley.Narcotics.Mon)$coef[17,4]
Summary.Table.Alley.Mon$MayPValue[4] <- summary(Fit.Alley.Battery.Mon)$coef[17,4]
Summary.Table.Alley.Mon$MayPValue[5] <- summary(Fit.Alley.CriminalDamage.Mon)$coef[17,4]
Summary.Table.Alley.Mon$MayPValue[6] <- summary(Fit.Alley.MotorVehicleTheft.Mon)$coef[17,4]
Summary.Table.Alley.Mon$MayPValue[7] <- summary(Fit.Alley.Robbery.Mon)$coef[17,4]
Summary.Table.Alley.Mon$MayPValue[8] <- summary(Fit.Alley.Assault.Mon)$coef[17,4]
Summary.Table.Alley.Mon$MayPValue[9] <- summary(Fit.Alley.Burglary.Mon)$coef[17,4]
Summary.Table.Alley.Mon$MayPValue[10] <- summary(Fit.Alley.Homicide.Mon)$coef[17,4]
Summary.Table.Alley.Mon$MayPValue[11] <- summary(Fit.Alley.DeceptivePractice.Mon)$coef[17,4]

Summary.Table.Alley.Mon$JunPValue[1] <- summary(Fit.Alley.AllCrimes.Mon)$coef[18,4]
Summary.Table.Alley.Mon$JunPValue[2] <- summary(Fit.Alley.Thefts.Mon)$coef[18,4]
Summary.Table.Alley.Mon$JunPValue[3] <- summary(Fit.Alley.Narcotics.Mon)$coef[18,4]
Summary.Table.Alley.Mon$JunPValue[4] <- summary(Fit.Alley.Battery.Mon)$coef[18,4]
Summary.Table.Alley.Mon$JunPValue[5] <- summary(Fit.Alley.CriminalDamage.Mon)$coef[18,4]
Summary.Table.Alley.Mon$JunPValue[6] <- summary(Fit.Alley.MotorVehicleTheft.Mon)$coef[18,4]
Summary.Table.Alley.Mon$JunPValue[7] <- summary(Fit.Alley.Robbery.Mon)$coef[18,4]
Summary.Table.Alley.Mon$JunPValue[8] <- summary(Fit.Alley.Assault.Mon)$coef[18,4]
Summary.Table.Alley.Mon$JunPValue[9] <- summary(Fit.Alley.Burglary.Mon)$coef[18,4]
Summary.Table.Alley.Mon$JunPValue[10] <- summary(Fit.Alley.Homicide.Mon)$coef[18,4]
Summary.Table.Alley.Mon$JunPValue[11] <- summary(Fit.Alley.DeceptivePractice.Mon)$coef[18,4]

Summary.Table.Alley.Mon$JulPValue[1] <- summary(Fit.Alley.AllCrimes.Mon)$coef[19,4]
Summary.Table.Alley.Mon$JulPValue[2] <- summary(Fit.Alley.Thefts.Mon)$coef[19,4]
Summary.Table.Alley.Mon$JulPValue[3] <- summary(Fit.Alley.Narcotics.Mon)$coef[19,4]
Summary.Table.Alley.Mon$JulPValue[4] <- summary(Fit.Alley.Battery.Mon)$coef[19,4]
Summary.Table.Alley.Mon$JulPValue[5] <- summary(Fit.Alley.CriminalDamage.Mon)$coef[19,4]
Summary.Table.Alley.Mon$JulPValue[6] <- summary(Fit.Alley.MotorVehicleTheft.Mon)$coef[19,4]
Summary.Table.Alley.Mon$JulPValue[7] <- summary(Fit.Alley.Robbery.Mon)$coef[19,4]
Summary.Table.Alley.Mon$JulPValue[8] <- summary(Fit.Alley.Assault.Mon)$coef[19,4]
Summary.Table.Alley.Mon$JulPValue[9] <- summary(Fit.Alley.Burglary.Mon)$coef[19,4]
Summary.Table.Alley.Mon$JulPValue[10] <- summary(Fit.Alley.Homicide.Mon)$coef[19,4]
Summary.Table.Alley.Mon$JulPValue[11] <- summary(Fit.Alley.DeceptivePractice.Mon)$coef[19,4]

Summary.Table.Alley.Mon$AugPValue[1] <- summary(Fit.Alley.AllCrimes.Mon)$coef[20,4]
Summary.Table.Alley.Mon$AugPValue[2] <- summary(Fit.Alley.Thefts.Mon)$coef[20,4]
Summary.Table.Alley.Mon$AugPValue[3] <- summary(Fit.Alley.Narcotics.Mon)$coef[20,4]
Summary.Table.Alley.Mon$AugPValue[4] <- summary(Fit.Alley.Battery.Mon)$coef[20,4]
Summary.Table.Alley.Mon$AugPValue[5] <- summary(Fit.Alley.CriminalDamage.Mon)$coef[20,4]
Summary.Table.Alley.Mon$AugPValue[6] <- summary(Fit.Alley.MotorVehicleTheft.Mon)$coef[20,4]
Summary.Table.Alley.Mon$AugPValue[7] <- summary(Fit.Alley.Robbery.Mon)$coef[20,4]
Summary.Table.Alley.Mon$AugPValue[8] <- summary(Fit.Alley.Assault.Mon)$coef[20,4]
Summary.Table.Alley.Mon$AugPValue[9] <- summary(Fit.Alley.Burglary.Mon)$coef[20,4]
Summary.Table.Alley.Mon$AugPValue[10] <- summary(Fit.Alley.Homicide.Mon)$coef[20,4]
Summary.Table.Alley.Mon$AugPValue[11] <- summary(Fit.Alley.DeceptivePractice.Mon)$coef[20,4]

Summary.Table.Alley.Mon$SepPValue[1] <- summary(Fit.Alley.AllCrimes.Mon)$coef[21,4]
Summary.Table.Alley.Mon$SepPValue[2] <- summary(Fit.Alley.Thefts.Mon)$coef[21,4]
Summary.Table.Alley.Mon$SepPValue[3] <- summary(Fit.Alley.Narcotics.Mon)$coef[21,4]
Summary.Table.Alley.Mon$SepPValue[4] <- summary(Fit.Alley.Battery.Mon)$coef[21,4]
Summary.Table.Alley.Mon$SepPValue[5] <- summary(Fit.Alley.CriminalDamage.Mon)$coef[21,4]
Summary.Table.Alley.Mon$SepPValue[6] <- summary(Fit.Alley.MotorVehicleTheft.Mon)$coef[21,4]
Summary.Table.Alley.Mon$SepPValue[7] <- summary(Fit.Alley.Robbery.Mon)$coef[21,4]
Summary.Table.Alley.Mon$SepPValue[8] <- summary(Fit.Alley.Assault.Mon)$coef[21,4]
Summary.Table.Alley.Mon$SepPValue[9] <- summary(Fit.Alley.Burglary.Mon)$coef[21,4]
Summary.Table.Alley.Mon$SepPValue[10] <- summary(Fit.Alley.Homicide.Mon)$coef[21,4]
Summary.Table.Alley.Mon$SepPValue[11] <- summary(Fit.Alley.DeceptivePractice.Mon)$coef[21,4]

Summary.Table.Alley.Mon$OctPValue[1] <- summary(Fit.Alley.AllCrimes.Mon)$coef[22,4]
Summary.Table.Alley.Mon$OctPValue[2] <- summary(Fit.Alley.Thefts.Mon)$coef[22,4]
Summary.Table.Alley.Mon$OctPValue[3] <- summary(Fit.Alley.Narcotics.Mon)$coef[22,4]
Summary.Table.Alley.Mon$OctPValue[4] <- summary(Fit.Alley.Battery.Mon)$coef[22,4]
Summary.Table.Alley.Mon$OctPValue[5] <- summary(Fit.Alley.CriminalDamage.Mon)$coef[22,4]
Summary.Table.Alley.Mon$OctPValue[6] <- summary(Fit.Alley.MotorVehicleTheft.Mon)$coef[22,4]
Summary.Table.Alley.Mon$OctPValue[7] <- summary(Fit.Alley.Robbery.Mon)$coef[22,4]
Summary.Table.Alley.Mon$OctPValue[8] <- summary(Fit.Alley.Assault.Mon)$coef[22,4]
Summary.Table.Alley.Mon$OctPValue[9] <- summary(Fit.Alley.Burglary.Mon)$coef[22,4]
Summary.Table.Alley.Mon$OctPValue[10] <- summary(Fit.Alley.Homicide.Mon)$coef[22,4]
Summary.Table.Alley.Mon$OctPValue[11] <- summary(Fit.Alley.DeceptivePractice.Mon)$coef[22,4]

Summary.Table.Alley.Mon$NovPValue[1] <- summary(Fit.Alley.AllCrimes.Mon)$coef[23,4]
Summary.Table.Alley.Mon$NovPValue[2] <- summary(Fit.Alley.Thefts.Mon)$coef[23,4]
Summary.Table.Alley.Mon$NovPValue[3] <- summary(Fit.Alley.Narcotics.Mon)$coef[23,4]
Summary.Table.Alley.Mon$NovPValue[4] <- summary(Fit.Alley.Battery.Mon)$coef[23,4]
Summary.Table.Alley.Mon$NovPValue[5] <- summary(Fit.Alley.CriminalDamage.Mon)$coef[23,4]
Summary.Table.Alley.Mon$NovPValue[6] <- summary(Fit.Alley.MotorVehicleTheft.Mon)$coef[23,4]
Summary.Table.Alley.Mon$NovPValue[7] <- summary(Fit.Alley.Robbery.Mon)$coef[23,4]
Summary.Table.Alley.Mon$NovPValue[8] <- summary(Fit.Alley.Assault.Mon)$coef[23,4]
Summary.Table.Alley.Mon$NovPValue[9] <- summary(Fit.Alley.Burglary.Mon)$coef[23,4]
Summary.Table.Alley.Mon$NovPValue[10] <- summary(Fit.Alley.Homicide.Mon)$coef[23,4]
Summary.Table.Alley.Mon$NovPValue[11] <- summary(Fit.Alley.DeceptivePractice.Mon)$coef[23,4]

Summary.Table.Alley.Mon$DecPValue[1] <- summary(Fit.Alley.AllCrimes.Mon)$coef[24,4]
Summary.Table.Alley.Mon$DecPValue[2] <- summary(Fit.Alley.Thefts.Mon)$coef[24,4]
Summary.Table.Alley.Mon$DecPValue[3] <- summary(Fit.Alley.Narcotics.Mon)$coef[24,4]
Summary.Table.Alley.Mon$DecPValue[4] <- summary(Fit.Alley.Battery.Mon)$coef[24,4]
Summary.Table.Alley.Mon$DecPValue[5] <- summary(Fit.Alley.CriminalDamage.Mon)$coef[24,4]
Summary.Table.Alley.Mon$DecPValue[6] <- summary(Fit.Alley.MotorVehicleTheft.Mon)$coef[24,4]
Summary.Table.Alley.Mon$DecPValue[7] <- summary(Fit.Alley.Robbery.Mon)$coef[24,4]
Summary.Table.Alley.Mon$DecPValue[8] <- summary(Fit.Alley.Assault.Mon)$coef[24,4]
Summary.Table.Alley.Mon$DecPValue[9] <- summary(Fit.Alley.Burglary.Mon)$coef[24,4]
Summary.Table.Alley.Mon$DecPValue[10] <- summary(Fit.Alley.Homicide.Mon)$coef[24,4]
Summary.Table.Alley.Mon$DecPValue[11] <- summary(Fit.Alley.DeceptivePractice.Mon)$coef[24,4]

Summary.Table.Alley.Mon$JanStar[which(Summary.Table.Alley.Mon$JanPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.Alley.Mon$JanPValue<0.01)))
Summary.Table.Alley.Mon$JanStar[which(Summary.Table.Alley.Mon$JanPValue<0.05 & Summary.Table.Alley.Mon$JanPValue>=0.01)] <- rep("*" , length(which(Summary.Table.Alley.Mon$JanPValue<0.05 & Summary.Table.Alley.Mon$JanPValue>=0.01)))
Summary.Table.Alley.Mon$FebStar[which(Summary.Table.Alley.Mon$FebPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.Alley.Mon$FebPValue<0.01)))
Summary.Table.Alley.Mon$FebStar[which(Summary.Table.Alley.Mon$FebPValue<0.05 & Summary.Table.Alley.Mon$FebPValue>=0.01)] <- rep("*" , length(which(Summary.Table.Alley.Mon$FebPValue<0.05 & Summary.Table.Alley.Mon$FebPValue>=0.01)))
Summary.Table.Alley.Mon$MarStar[which(Summary.Table.Alley.Mon$MarPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.Alley.Mon$MarPValue<0.01)))
Summary.Table.Alley.Mon$MarStar[which(Summary.Table.Alley.Mon$MarPValue<0.05 & Summary.Table.Alley.Mon$MarPValue>=0.01)] <- rep("*" , length(which(Summary.Table.Alley.Mon$MarPValue<0.05 & Summary.Table.Alley.Mon$MarPValue>=0.01)))
Summary.Table.Alley.Mon$AprStar[which(Summary.Table.Alley.Mon$AprPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.Alley.Mon$AprPValue<0.01)))
Summary.Table.Alley.Mon$AprStar[which(Summary.Table.Alley.Mon$AprPValue<0.05 & Summary.Table.Alley.Mon$AprPValue>=0.01)] <- rep("*" , length(which(Summary.Table.Alley.Mon$AprPValue<0.05 & Summary.Table.Alley.Mon$AprPValue>=0.01)))
Summary.Table.Alley.Mon$MayStar[which(Summary.Table.Alley.Mon$MayPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.Alley.Mon$MayPValue<0.01)))
Summary.Table.Alley.Mon$MayStar[which(Summary.Table.Alley.Mon$MayPValue<0.05 & Summary.Table.Alley.Mon$MayPValue>=0.01)] <- rep("*" , length(which(Summary.Table.Alley.Mon$MayPValue<0.05 & Summary.Table.Alley.Mon$MayPValue>=0.01)))
Summary.Table.Alley.Mon$JunStar[which(Summary.Table.Alley.Mon$JunPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.Alley.Mon$JunPValue<0.01)))
Summary.Table.Alley.Mon$JunStar[which(Summary.Table.Alley.Mon$JunPValue<0.05 & Summary.Table.Alley.Mon$JunPValue>=0.01)] <- rep("*" , length(which(Summary.Table.Alley.Mon$JunPValue<0.05 & Summary.Table.Alley.Mon$JunPValue>=0.01)))
Summary.Table.Alley.Mon$JulStar[which(Summary.Table.Alley.Mon$JulPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.Alley.Mon$JulPValue<0.01)))
Summary.Table.Alley.Mon$JulStar[which(Summary.Table.Alley.Mon$JulPValue<0.05 & Summary.Table.Alley.Mon$JulPValue>=0.01)] <- rep("*" , length(which(Summary.Table.Alley.Mon$JulPValue<0.05 & Summary.Table.Alley.Mon$JulPValue>=0.01)))
Summary.Table.Alley.Mon$AugStar[which(Summary.Table.Alley.Mon$AugPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.Alley.Mon$AugPValue<0.01)))
Summary.Table.Alley.Mon$AugStar[which(Summary.Table.Alley.Mon$AugPValue<0.05 & Summary.Table.Alley.Mon$AugPValue>=0.01)] <- rep("*" , length(which(Summary.Table.Alley.Mon$AugPValue<0.05 & Summary.Table.Alley.Mon$AugPValue>=0.01)))
Summary.Table.Alley.Mon$SepStar[which(Summary.Table.Alley.Mon$SepPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.Alley.Mon$SepPValue<0.01)))
Summary.Table.Alley.Mon$SepStar[which(Summary.Table.Alley.Mon$SepPValue<0.05 & Summary.Table.Alley.Mon$SepPValue>=0.01)] <- rep("*" , length(which(Summary.Table.Alley.Mon$SepPValue<0.05 & Summary.Table.Alley.Mon$SepPValue>=0.01)))
Summary.Table.Alley.Mon$OctStar[which(Summary.Table.Alley.Mon$OctPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.Alley.Mon$OctPValue<0.01)))
Summary.Table.Alley.Mon$OctStar[which(Summary.Table.Alley.Mon$OctPValue<0.05 & Summary.Table.Alley.Mon$OctPValue>=0.01)] <- rep("*" , length(which(Summary.Table.Alley.Mon$OctPValue<0.05 & Summary.Table.Alley.Mon$OctPValue>=0.01)))
Summary.Table.Alley.Mon$NovStar[which(Summary.Table.Alley.Mon$NovPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.Alley.Mon$NovPValue<0.01)))
Summary.Table.Alley.Mon$NovStar[which(Summary.Table.Alley.Mon$NovPValue<0.05 & Summary.Table.Alley.Mon$NovPValue>=0.01)] <- rep("*" , length(which(Summary.Table.Alley.Mon$NovPValue<0.05 & Summary.Table.Alley.Mon$NovPValue>=0.01)))
Summary.Table.Alley.Mon$DecStar[which(Summary.Table.Alley.Mon$DecPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.Alley.Mon$DecPValue<0.01)))
Summary.Table.Alley.Mon$DecStar[which(Summary.Table.Alley.Mon$DecPValue<0.05 & Summary.Table.Alley.Mon$DecPValue>=0.01)] <- rep("*" , length(which(Summary.Table.Alley.Mon$DecPValue<0.05 & Summary.Table.Alley.Mon$DecPValue>=0.01)))
                                                                                                                                             
                   
                                                                                                                                             
Street.Lights.OneOut.Pois$JanInd <- Street.Lights.OneOut.Pois$Jan * Street.Lights.OneOut.Pois$OutageInd
Street.Lights.OneOut.Pois$FebInd <- Street.Lights.OneOut.Pois$Feb * Street.Lights.OneOut.Pois$OutageInd
Street.Lights.OneOut.Pois$MarInd <- Street.Lights.OneOut.Pois$Mar * Street.Lights.OneOut.Pois$OutageInd
Street.Lights.OneOut.Pois$AprInd <- Street.Lights.OneOut.Pois$Apr * Street.Lights.OneOut.Pois$OutageInd
Street.Lights.OneOut.Pois$MayInd <- Street.Lights.OneOut.Pois$May * Street.Lights.OneOut.Pois$OutageInd
Street.Lights.OneOut.Pois$JunInd <- Street.Lights.OneOut.Pois$Jun * Street.Lights.OneOut.Pois$OutageInd
Street.Lights.OneOut.Pois$JulInd <- Street.Lights.OneOut.Pois$Jul * Street.Lights.OneOut.Pois$OutageInd
Street.Lights.OneOut.Pois$AugInd <- Street.Lights.OneOut.Pois$Aug * Street.Lights.OneOut.Pois$OutageInd
Street.Lights.OneOut.Pois$SepInd <- Street.Lights.OneOut.Pois$Sep * Street.Lights.OneOut.Pois$OutageInd
Street.Lights.OneOut.Pois$OctInd <- Street.Lights.OneOut.Pois$Oct * Street.Lights.OneOut.Pois$OutageInd
Street.Lights.OneOut.Pois$NovInd <- Street.Lights.OneOut.Pois$Nov * Street.Lights.OneOut.Pois$OutageInd
Street.Lights.OneOut.Pois$DecInd <- Street.Lights.OneOut.Pois$Dec * Street.Lights.OneOut.Pois$OutageInd

Summary.Table.OneOut.Mon  <- data.frame(matrix(ncol = 36, nrow = 11), row.names = c("All Crimes (No Deceptive Practice)", 
                                                                                   "Thefts", "Narcotics", "Battery", "Criminal Damage", "Motor Vehicle Theft", 
                                                                                   "Robbery", "Assault", "Burglary", "Homicide", "Deceptive Practice"))

Summary.Table.OneOut.Mon  <- rename(Summary.Table.OneOut.Mon,      c("X1"="JanEffect",  "X2"="JanPValue",  "X3"="JanStar", 
                                                                   "X4"="FebEffect",  "X5"="FebPValue",  "X6"="FebStar",
                                                                   "X7"="MarEffect",  "X8"="MarPValue",  "X9"="MarStar",
                                                                   "X10"="AprEffect", "X11"="AprPValue", "X12"="AprStar",
                                                                   "X13"="MayEffect", "X14"="MayPValue", "X15"="MayStar",
                                                                   "X16"="JunEffect", "X17"="JunPValue", "X18"="JunStar",
                                                                   "X19"="JulEffect", "X20"="JulPValue", "X21"="JulStar",
                                                                   "X22"="AugEffect", "X23"="AugPValue", "X24"="AugStar",
                                                                   "X25"="SepEffect", "X26"="SepPValue", "X27"="SepStar",
                                                                   "X28"="OctEffect", "X29"="OctPValue", "X30"="OctStar",
                                                                   "X31"="NovEffect", "X32"="NovPValue", "X33"="NovStar",
                                                                   "X34"="DecEffect", "X35"="DecPValue", "X36"="DecStar"))



Fit.OneOut.AllCrimes.Mon         <- glm(AllCrimes         ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Thefts.Mon            <- glm(Thefts            ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Narcotics.Mon         <- glm(Narcotics         ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Battery.Mon           <- glm(Battery           ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.CriminalDamage.Mon    <- glm(CriminalDamage    ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.MotorVehicleTheft.Mon <- glm(MotorVehicleTheft ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Robbery.Mon           <- glm(Robbery           ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Assault.Mon           <- glm(Assault           ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Burglary.Mon          <- glm(Burglary          ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.Homicide.Mon          <- glm(Homicide          ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)
Fit.OneOut.DeceptivePractice.Mon <- glm(DeceptivePractice ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.OneOut.Pois)

Summary.Table.OneOut.Mon$JanStar <- character(nrow(Summary.Table.OneOut.Mon))
Summary.Table.OneOut.Mon$FebStar <- character(nrow(Summary.Table.OneOut.Mon))
Summary.Table.OneOut.Mon$MarStar <- character(nrow(Summary.Table.OneOut.Mon))
Summary.Table.OneOut.Mon$AprStar <- character(nrow(Summary.Table.OneOut.Mon))
Summary.Table.OneOut.Mon$MayStar <- character(nrow(Summary.Table.OneOut.Mon))
Summary.Table.OneOut.Mon$JunStar <- character(nrow(Summary.Table.OneOut.Mon))
Summary.Table.OneOut.Mon$JulStar <- character(nrow(Summary.Table.OneOut.Mon))
Summary.Table.OneOut.Mon$AugStar <- character(nrow(Summary.Table.OneOut.Mon))
Summary.Table.OneOut.Mon$SepStar <- character(nrow(Summary.Table.OneOut.Mon))
Summary.Table.OneOut.Mon$OctStar <- character(nrow(Summary.Table.OneOut.Mon))
Summary.Table.OneOut.Mon$NovStar <- character(nrow(Summary.Table.OneOut.Mon))
Summary.Table.OneOut.Mon$DecStar <- character(nrow(Summary.Table.OneOut.Mon))

Summary.Table.OneOut.Mon$JanEffect[1] <- 100*exp(Fit.OneOut.AllCrimes.Mon$coef[13])-100
Summary.Table.OneOut.Mon$JanEffect[2] <- 100*exp(Fit.OneOut.Thefts.Mon$coef[13])-100
Summary.Table.OneOut.Mon$JanEffect[3] <- 100*exp(Fit.OneOut.Narcotics.Mon$coef[13])-100
Summary.Table.OneOut.Mon$JanEffect[4] <- 100*exp(Fit.OneOut.Battery.Mon$coef[13])-100
Summary.Table.OneOut.Mon$JanEffect[5] <- 100*exp(Fit.OneOut.CriminalDamage.Mon$coef[13])-100
Summary.Table.OneOut.Mon$JanEffect[6] <- 100*exp(Fit.OneOut.MotorVehicleTheft.Mon$coef[13])-100
Summary.Table.OneOut.Mon$JanEffect[7] <- 100*exp(Fit.OneOut.Robbery.Mon$coef[13])-100
Summary.Table.OneOut.Mon$JanEffect[8] <- 100*exp(Fit.OneOut.Assault.Mon$coef[13])-100
Summary.Table.OneOut.Mon$JanEffect[9] <- 100*exp(Fit.OneOut.Burglary.Mon$coef[13])-100
Summary.Table.OneOut.Mon$JanEffect[10] <- 100*exp(Fit.OneOut.Homicide.Mon$coef[13])-100
Summary.Table.OneOut.Mon$JanEffect[11] <- 100*exp(Fit.OneOut.DeceptivePractice.Mon$coef[13])-100

Summary.Table.OneOut.Mon$FebEffect[1] <- 100*exp(Fit.OneOut.AllCrimes.Mon$coef[14])-100
Summary.Table.OneOut.Mon$FebEffect[2] <- 100*exp(Fit.OneOut.Thefts.Mon$coef[14])-100
Summary.Table.OneOut.Mon$FebEffect[3] <- 100*exp(Fit.OneOut.Narcotics.Mon$coef[14])-100
Summary.Table.OneOut.Mon$FebEffect[4] <- 100*exp(Fit.OneOut.Battery.Mon$coef[14])-100
Summary.Table.OneOut.Mon$FebEffect[5] <- 100*exp(Fit.OneOut.CriminalDamage.Mon$coef[14])-100
Summary.Table.OneOut.Mon$FebEffect[6] <- 100*exp(Fit.OneOut.MotorVehicleTheft.Mon$coef[14])-100
Summary.Table.OneOut.Mon$FebEffect[7] <- 100*exp(Fit.OneOut.Robbery.Mon$coef[14])-100
Summary.Table.OneOut.Mon$FebEffect[8] <- 100*exp(Fit.OneOut.Assault.Mon$coef[14])-100
Summary.Table.OneOut.Mon$FebEffect[9] <- 100*exp(Fit.OneOut.Burglary.Mon$coef[14])-100
Summary.Table.OneOut.Mon$FebEffect[10] <- 100*exp(Fit.OneOut.Homicide.Mon$coef[14])-100
Summary.Table.OneOut.Mon$FebEffect[11] <- 100*exp(Fit.OneOut.DeceptivePractice.Mon$coef[14])-100

Summary.Table.OneOut.Mon$MarEffect[1] <- 100*exp(Fit.OneOut.AllCrimes.Mon$coef[15])-100
Summary.Table.OneOut.Mon$MarEffect[2] <- 100*exp(Fit.OneOut.Thefts.Mon$coef[15])-100
Summary.Table.OneOut.Mon$MarEffect[3] <- 100*exp(Fit.OneOut.Narcotics.Mon$coef[15])-100
Summary.Table.OneOut.Mon$MarEffect[4] <- 100*exp(Fit.OneOut.Battery.Mon$coef[15])-100
Summary.Table.OneOut.Mon$MarEffect[5] <- 100*exp(Fit.OneOut.CriminalDamage.Mon$coef[15])-100
Summary.Table.OneOut.Mon$MarEffect[6] <- 100*exp(Fit.OneOut.MotorVehicleTheft.Mon$coef[15])-100
Summary.Table.OneOut.Mon$MarEffect[7] <- 100*exp(Fit.OneOut.Robbery.Mon$coef[15])-100
Summary.Table.OneOut.Mon$MarEffect[8] <- 100*exp(Fit.OneOut.Assault.Mon$coef[15])-100
Summary.Table.OneOut.Mon$MarEffect[9] <- 100*exp(Fit.OneOut.Burglary.Mon$coef[15])-100
Summary.Table.OneOut.Mon$MarEffect[10] <- 100*exp(Fit.OneOut.Homicide.Mon$coef[15])-100
Summary.Table.OneOut.Mon$MarEffect[11] <- 100*exp(Fit.OneOut.DeceptivePractice.Mon$coef[15])-100

Summary.Table.OneOut.Mon$AprEffect[1] <- 100*exp(Fit.OneOut.AllCrimes.Mon$coef[16])-100
Summary.Table.OneOut.Mon$AprEffect[2] <- 100*exp(Fit.OneOut.Thefts.Mon$coef[16])-100
Summary.Table.OneOut.Mon$AprEffect[3] <- 100*exp(Fit.OneOut.Narcotics.Mon$coef[16])-100
Summary.Table.OneOut.Mon$AprEffect[4] <- 100*exp(Fit.OneOut.Battery.Mon$coef[16])-100
Summary.Table.OneOut.Mon$AprEffect[5] <- 100*exp(Fit.OneOut.CriminalDamage.Mon$coef[16])-100
Summary.Table.OneOut.Mon$AprEffect[6] <- 100*exp(Fit.OneOut.MotorVehicleTheft.Mon$coef[16])-100
Summary.Table.OneOut.Mon$AprEffect[7] <- 100*exp(Fit.OneOut.Robbery.Mon$coef[16])-100
Summary.Table.OneOut.Mon$AprEffect[8] <- 100*exp(Fit.OneOut.Assault.Mon$coef[16])-100
Summary.Table.OneOut.Mon$AprEffect[9] <- 100*exp(Fit.OneOut.Burglary.Mon$coef[16])-100
Summary.Table.OneOut.Mon$AprEffect[10] <- 100*exp(Fit.OneOut.Homicide.Mon$coef[16])-100
Summary.Table.OneOut.Mon$AprEffect[11] <- 100*exp(Fit.OneOut.DeceptivePractice.Mon$coef[16])-100

Summary.Table.OneOut.Mon$MayEffect[1] <- 100*exp(Fit.OneOut.AllCrimes.Mon$coef[17])-100
Summary.Table.OneOut.Mon$MayEffect[2] <- 100*exp(Fit.OneOut.Thefts.Mon$coef[17])-100
Summary.Table.OneOut.Mon$MayEffect[3] <- 100*exp(Fit.OneOut.Narcotics.Mon$coef[17])-100
Summary.Table.OneOut.Mon$MayEffect[4] <- 100*exp(Fit.OneOut.Battery.Mon$coef[17])-100
Summary.Table.OneOut.Mon$MayEffect[5] <- 100*exp(Fit.OneOut.CriminalDamage.Mon$coef[17])-100
Summary.Table.OneOut.Mon$MayEffect[6] <- 100*exp(Fit.OneOut.MotorVehicleTheft.Mon$coef[17])-100
Summary.Table.OneOut.Mon$MayEffect[7] <- 100*exp(Fit.OneOut.Robbery.Mon$coef[17])-100
Summary.Table.OneOut.Mon$MayEffect[8] <- 100*exp(Fit.OneOut.Assault.Mon$coef[17])-100
Summary.Table.OneOut.Mon$MayEffect[9] <- 100*exp(Fit.OneOut.Burglary.Mon$coef[17])-100
Summary.Table.OneOut.Mon$MayEffect[10] <- 100*exp(Fit.OneOut.Homicide.Mon$coef[17])-100
Summary.Table.OneOut.Mon$MayEffect[11] <- 100*exp(Fit.OneOut.DeceptivePractice.Mon$coef[17])-100

Summary.Table.OneOut.Mon$JunEffect[1] <- 100*exp(Fit.OneOut.AllCrimes.Mon$coef[18])-100
Summary.Table.OneOut.Mon$JunEffect[2] <- 100*exp(Fit.OneOut.Thefts.Mon$coef[18])-100
Summary.Table.OneOut.Mon$JunEffect[3] <- 100*exp(Fit.OneOut.Narcotics.Mon$coef[18])-100
Summary.Table.OneOut.Mon$JunEffect[4] <- 100*exp(Fit.OneOut.Battery.Mon$coef[18])-100
Summary.Table.OneOut.Mon$JunEffect[5] <- 100*exp(Fit.OneOut.CriminalDamage.Mon$coef[18])-100
Summary.Table.OneOut.Mon$JunEffect[6] <- 100*exp(Fit.OneOut.MotorVehicleTheft.Mon$coef[18])-100
Summary.Table.OneOut.Mon$JunEffect[7] <- 100*exp(Fit.OneOut.Robbery.Mon$coef[18])-100
Summary.Table.OneOut.Mon$JunEffect[8] <- 100*exp(Fit.OneOut.Assault.Mon$coef[18])-100
Summary.Table.OneOut.Mon$JunEffect[9] <- 100*exp(Fit.OneOut.Burglary.Mon$coef[18])-100
Summary.Table.OneOut.Mon$JunEffect[10] <- 100*exp(Fit.OneOut.Homicide.Mon$coef[18])-100
Summary.Table.OneOut.Mon$JunEffect[11] <- 100*exp(Fit.OneOut.DeceptivePractice.Mon$coef[18])-100

Summary.Table.OneOut.Mon$JulEffect[1] <- 100*exp(Fit.OneOut.AllCrimes.Mon$coef[19])-100
Summary.Table.OneOut.Mon$JulEffect[2] <- 100*exp(Fit.OneOut.Thefts.Mon$coef[19])-100
Summary.Table.OneOut.Mon$JulEffect[3] <- 100*exp(Fit.OneOut.Narcotics.Mon$coef[19])-100
Summary.Table.OneOut.Mon$JulEffect[4] <- 100*exp(Fit.OneOut.Battery.Mon$coef[19])-100
Summary.Table.OneOut.Mon$JulEffect[5] <- 100*exp(Fit.OneOut.CriminalDamage.Mon$coef[19])-100
Summary.Table.OneOut.Mon$JulEffect[6] <- 100*exp(Fit.OneOut.MotorVehicleTheft.Mon$coef[19])-100
Summary.Table.OneOut.Mon$JulEffect[7] <- 100*exp(Fit.OneOut.Robbery.Mon$coef[19])-100
Summary.Table.OneOut.Mon$JulEffect[8] <- 100*exp(Fit.OneOut.Assault.Mon$coef[19])-100
Summary.Table.OneOut.Mon$JulEffect[9] <- 100*exp(Fit.OneOut.Burglary.Mon$coef[19])-100
Summary.Table.OneOut.Mon$JulEffect[10] <- 100*exp(Fit.OneOut.Homicide.Mon$coef[19])-100
Summary.Table.OneOut.Mon$JulEffect[11] <- 100*exp(Fit.OneOut.DeceptivePractice.Mon$coef[19])-100

Summary.Table.OneOut.Mon$AugEffect[1] <- 100*exp(Fit.OneOut.AllCrimes.Mon$coef[20])-100
Summary.Table.OneOut.Mon$AugEffect[2] <- 100*exp(Fit.OneOut.Thefts.Mon$coef[20])-100
Summary.Table.OneOut.Mon$AugEffect[3] <- 100*exp(Fit.OneOut.Narcotics.Mon$coef[20])-100
Summary.Table.OneOut.Mon$AugEffect[4] <- 100*exp(Fit.OneOut.Battery.Mon$coef[20])-100
Summary.Table.OneOut.Mon$AugEffect[5] <- 100*exp(Fit.OneOut.CriminalDamage.Mon$coef[20])-100
Summary.Table.OneOut.Mon$AugEffect[6] <- 100*exp(Fit.OneOut.MotorVehicleTheft.Mon$coef[20])-100
Summary.Table.OneOut.Mon$AugEffect[7] <- 100*exp(Fit.OneOut.Robbery.Mon$coef[20])-100
Summary.Table.OneOut.Mon$AugEffect[8] <- 100*exp(Fit.OneOut.Assault.Mon$coef[20])-100
Summary.Table.OneOut.Mon$AugEffect[9] <- 100*exp(Fit.OneOut.Burglary.Mon$coef[20])-100
Summary.Table.OneOut.Mon$AugEffect[10] <- 100*exp(Fit.OneOut.Homicide.Mon$coef[20])-100
Summary.Table.OneOut.Mon$AugEffect[11] <- 100*exp(Fit.OneOut.DeceptivePractice.Mon$coef[20])-100

Summary.Table.OneOut.Mon$SepEffect[1] <- 100*exp(Fit.OneOut.AllCrimes.Mon$coef[21])-100
Summary.Table.OneOut.Mon$SepEffect[2] <- 100*exp(Fit.OneOut.Thefts.Mon$coef[21])-100
Summary.Table.OneOut.Mon$SepEffect[3] <- 100*exp(Fit.OneOut.Narcotics.Mon$coef[21])-100
Summary.Table.OneOut.Mon$SepEffect[4] <- 100*exp(Fit.OneOut.Battery.Mon$coef[21])-100
Summary.Table.OneOut.Mon$SepEffect[5] <- 100*exp(Fit.OneOut.CriminalDamage.Mon$coef[21])-100
Summary.Table.OneOut.Mon$SepEffect[6] <- 100*exp(Fit.OneOut.MotorVehicleTheft.Mon$coef[21])-100
Summary.Table.OneOut.Mon$SepEffect[7] <- 100*exp(Fit.OneOut.Robbery.Mon$coef[21])-100
Summary.Table.OneOut.Mon$SepEffect[8] <- 100*exp(Fit.OneOut.Assault.Mon$coef[21])-100
Summary.Table.OneOut.Mon$SepEffect[9] <- 100*exp(Fit.OneOut.Burglary.Mon$coef[21])-100
Summary.Table.OneOut.Mon$SepEffect[10] <- 100*exp(Fit.OneOut.Homicide.Mon$coef[21])-100
Summary.Table.OneOut.Mon$SepEffect[11] <- 100*exp(Fit.OneOut.DeceptivePractice.Mon$coef[21])-100

Summary.Table.OneOut.Mon$OctEffect[1] <- 100*exp(Fit.OneOut.AllCrimes.Mon$coef[22])-100
Summary.Table.OneOut.Mon$OctEffect[2] <- 100*exp(Fit.OneOut.Thefts.Mon$coef[22])-100
Summary.Table.OneOut.Mon$OctEffect[3] <- 100*exp(Fit.OneOut.Narcotics.Mon$coef[22])-100
Summary.Table.OneOut.Mon$OctEffect[4] <- 100*exp(Fit.OneOut.Battery.Mon$coef[22])-100
Summary.Table.OneOut.Mon$OctEffect[5] <- 100*exp(Fit.OneOut.CriminalDamage.Mon$coef[22])-100
Summary.Table.OneOut.Mon$OctEffect[6] <- 100*exp(Fit.OneOut.MotorVehicleTheft.Mon$coef[22])-100
Summary.Table.OneOut.Mon$OctEffect[7] <- 100*exp(Fit.OneOut.Robbery.Mon$coef[22])-100
Summary.Table.OneOut.Mon$OctEffect[8] <- 100*exp(Fit.OneOut.Assault.Mon$coef[22])-100
Summary.Table.OneOut.Mon$OctEffect[9] <- 100*exp(Fit.OneOut.Burglary.Mon$coef[22])-100
Summary.Table.OneOut.Mon$OctEffect[10] <- 100*exp(Fit.OneOut.Homicide.Mon$coef[22])-100
Summary.Table.OneOut.Mon$OctEffect[11] <- 100*exp(Fit.OneOut.DeceptivePractice.Mon$coef[22])-100

Summary.Table.OneOut.Mon$NovEffect[1] <- 100*exp(Fit.OneOut.AllCrimes.Mon$coef[23])-100
Summary.Table.OneOut.Mon$NovEffect[2] <- 100*exp(Fit.OneOut.Thefts.Mon$coef[23])-100
Summary.Table.OneOut.Mon$NovEffect[3] <- 100*exp(Fit.OneOut.Narcotics.Mon$coef[23])-100
Summary.Table.OneOut.Mon$NovEffect[4] <- 100*exp(Fit.OneOut.Battery.Mon$coef[23])-100
Summary.Table.OneOut.Mon$NovEffect[5] <- 100*exp(Fit.OneOut.CriminalDamage.Mon$coef[23])-100
Summary.Table.OneOut.Mon$NovEffect[6] <- 100*exp(Fit.OneOut.MotorVehicleTheft.Mon$coef[23])-100
Summary.Table.OneOut.Mon$NovEffect[7] <- 100*exp(Fit.OneOut.Robbery.Mon$coef[23])-100
Summary.Table.OneOut.Mon$NovEffect[8] <- 100*exp(Fit.OneOut.Assault.Mon$coef[23])-100
Summary.Table.OneOut.Mon$NovEffect[9] <- 100*exp(Fit.OneOut.Burglary.Mon$coef[23])-100
Summary.Table.OneOut.Mon$NovEffect[10] <- 100*exp(Fit.OneOut.Homicide.Mon$coef[23])-100
Summary.Table.OneOut.Mon$NovEffect[11] <- 100*exp(Fit.OneOut.DeceptivePractice.Mon$coef[23])-100

Summary.Table.OneOut.Mon$DecEffect[1] <- 100*exp(Fit.OneOut.AllCrimes.Mon$coef[24])-100
Summary.Table.OneOut.Mon$DecEffect[2] <- 100*exp(Fit.OneOut.Thefts.Mon$coef[24])-100
Summary.Table.OneOut.Mon$DecEffect[3] <- 100*exp(Fit.OneOut.Narcotics.Mon$coef[24])-100
Summary.Table.OneOut.Mon$DecEffect[4] <- 100*exp(Fit.OneOut.Battery.Mon$coef[24])-100
Summary.Table.OneOut.Mon$DecEffect[5] <- 100*exp(Fit.OneOut.CriminalDamage.Mon$coef[24])-100
Summary.Table.OneOut.Mon$DecEffect[6] <- 100*exp(Fit.OneOut.MotorVehicleTheft.Mon$coef[24])-100
Summary.Table.OneOut.Mon$DecEffect[7] <- 100*exp(Fit.OneOut.Robbery.Mon$coef[24])-100
Summary.Table.OneOut.Mon$DecEffect[8] <- 100*exp(Fit.OneOut.Assault.Mon$coef[24])-100
Summary.Table.OneOut.Mon$DecEffect[9] <- 100*exp(Fit.OneOut.Burglary.Mon$coef[24])-100
Summary.Table.OneOut.Mon$DecEffect[10] <- 100*exp(Fit.OneOut.Homicide.Mon$coef[24])-100
Summary.Table.OneOut.Mon$DecEffect[11] <- 100*exp(Fit.OneOut.DeceptivePractice.Mon$coef[24])-100


Summary.Table.OneOut.Mon$JanPValue[1] <- summary(Fit.OneOut.AllCrimes.Mon)$coef[13,4]
Summary.Table.OneOut.Mon$JanPValue[2] <- summary(Fit.OneOut.Thefts.Mon)$coef[13,4]
Summary.Table.OneOut.Mon$JanPValue[3] <- summary(Fit.OneOut.Narcotics.Mon)$coef[13,4]
Summary.Table.OneOut.Mon$JanPValue[4] <- summary(Fit.OneOut.Battery.Mon)$coef[13,4]
Summary.Table.OneOut.Mon$JanPValue[5] <- summary(Fit.OneOut.CriminalDamage.Mon)$coef[13,4]
Summary.Table.OneOut.Mon$JanPValue[6] <- summary(Fit.OneOut.MotorVehicleTheft.Mon)$coef[13,4]
Summary.Table.OneOut.Mon$JanPValue[7] <- summary(Fit.OneOut.Robbery.Mon)$coef[13,4]
Summary.Table.OneOut.Mon$JanPValue[8] <- summary(Fit.OneOut.Assault.Mon)$coef[13,4]
Summary.Table.OneOut.Mon$JanPValue[9] <- summary(Fit.OneOut.Burglary.Mon)$coef[13,4]
Summary.Table.OneOut.Mon$JanPValue[10] <- summary(Fit.OneOut.Homicide.Mon)$coef[13,4]
Summary.Table.OneOut.Mon$JanPValue[11] <- summary(Fit.OneOut.DeceptivePractice.Mon)$coef[13,4]

Summary.Table.OneOut.Mon$FebPValue[1] <- summary(Fit.OneOut.AllCrimes.Mon)$coef[14,4]
Summary.Table.OneOut.Mon$FebPValue[2] <- summary(Fit.OneOut.Thefts.Mon)$coef[14,4]
Summary.Table.OneOut.Mon$FebPValue[3] <- summary(Fit.OneOut.Narcotics.Mon)$coef[14,4]
Summary.Table.OneOut.Mon$FebPValue[4] <- summary(Fit.OneOut.Battery.Mon)$coef[14,4]
Summary.Table.OneOut.Mon$FebPValue[5] <- summary(Fit.OneOut.CriminalDamage.Mon)$coef[14,4]
Summary.Table.OneOut.Mon$FebPValue[6] <- summary(Fit.OneOut.MotorVehicleTheft.Mon)$coef[14,4]
Summary.Table.OneOut.Mon$FebPValue[7] <- summary(Fit.OneOut.Robbery.Mon)$coef[14,4]
Summary.Table.OneOut.Mon$FebPValue[8] <- summary(Fit.OneOut.Assault.Mon)$coef[14,4]
Summary.Table.OneOut.Mon$FebPValue[9] <- summary(Fit.OneOut.Burglary.Mon)$coef[14,4]
Summary.Table.OneOut.Mon$FebPValue[10] <- summary(Fit.OneOut.Homicide.Mon)$coef[14,4]
Summary.Table.OneOut.Mon$FebPValue[11] <- summary(Fit.OneOut.DeceptivePractice.Mon)$coef[14,4]

Summary.Table.OneOut.Mon$MarPValue[1] <- summary(Fit.OneOut.AllCrimes.Mon)$coef[15,4]
Summary.Table.OneOut.Mon$MarPValue[2] <- summary(Fit.OneOut.Thefts.Mon)$coef[15,4]
Summary.Table.OneOut.Mon$MarPValue[3] <- summary(Fit.OneOut.Narcotics.Mon)$coef[15,4]
Summary.Table.OneOut.Mon$MarPValue[4] <- summary(Fit.OneOut.Battery.Mon)$coef[15,4]
Summary.Table.OneOut.Mon$MarPValue[5] <- summary(Fit.OneOut.CriminalDamage.Mon)$coef[15,4]
Summary.Table.OneOut.Mon$MarPValue[6] <- summary(Fit.OneOut.MotorVehicleTheft.Mon)$coef[15,4]
Summary.Table.OneOut.Mon$MarPValue[7] <- summary(Fit.OneOut.Robbery.Mon)$coef[15,4]
Summary.Table.OneOut.Mon$MarPValue[8] <- summary(Fit.OneOut.Assault.Mon)$coef[15,4]
Summary.Table.OneOut.Mon$MarPValue[9] <- summary(Fit.OneOut.Burglary.Mon)$coef[15,4]
Summary.Table.OneOut.Mon$MarPValue[10] <- summary(Fit.OneOut.Homicide.Mon)$coef[15,4]
Summary.Table.OneOut.Mon$MarPValue[11] <- summary(Fit.OneOut.DeceptivePractice.Mon)$coef[15,4]

Summary.Table.OneOut.Mon$AprPValue[1] <- summary(Fit.OneOut.AllCrimes.Mon)$coef[16,4]
Summary.Table.OneOut.Mon$AprPValue[2] <- summary(Fit.OneOut.Thefts.Mon)$coef[16,4]
Summary.Table.OneOut.Mon$AprPValue[3] <- summary(Fit.OneOut.Narcotics.Mon)$coef[16,4]
Summary.Table.OneOut.Mon$AprPValue[4] <- summary(Fit.OneOut.Battery.Mon)$coef[16,4]
Summary.Table.OneOut.Mon$AprPValue[5] <- summary(Fit.OneOut.CriminalDamage.Mon)$coef[16,4]
Summary.Table.OneOut.Mon$AprPValue[6] <- summary(Fit.OneOut.MotorVehicleTheft.Mon)$coef[16,4]
Summary.Table.OneOut.Mon$AprPValue[7] <- summary(Fit.OneOut.Robbery.Mon)$coef[16,4]
Summary.Table.OneOut.Mon$AprPValue[8] <- summary(Fit.OneOut.Assault.Mon)$coef[16,4]
Summary.Table.OneOut.Mon$AprPValue[9] <- summary(Fit.OneOut.Burglary.Mon)$coef[16,4]
Summary.Table.OneOut.Mon$AprPValue[10] <- summary(Fit.OneOut.Homicide.Mon)$coef[16,4]
Summary.Table.OneOut.Mon$AprPValue[11] <- summary(Fit.OneOut.DeceptivePractice.Mon)$coef[16,4]

Summary.Table.OneOut.Mon$MayPValue[1] <- summary(Fit.OneOut.AllCrimes.Mon)$coef[17,4]
Summary.Table.OneOut.Mon$MayPValue[2] <- summary(Fit.OneOut.Thefts.Mon)$coef[17,4]
Summary.Table.OneOut.Mon$MayPValue[3] <- summary(Fit.OneOut.Narcotics.Mon)$coef[17,4]
Summary.Table.OneOut.Mon$MayPValue[4] <- summary(Fit.OneOut.Battery.Mon)$coef[17,4]
Summary.Table.OneOut.Mon$MayPValue[5] <- summary(Fit.OneOut.CriminalDamage.Mon)$coef[17,4]
Summary.Table.OneOut.Mon$MayPValue[6] <- summary(Fit.OneOut.MotorVehicleTheft.Mon)$coef[17,4]
Summary.Table.OneOut.Mon$MayPValue[7] <- summary(Fit.OneOut.Robbery.Mon)$coef[17,4]
Summary.Table.OneOut.Mon$MayPValue[8] <- summary(Fit.OneOut.Assault.Mon)$coef[17,4]
Summary.Table.OneOut.Mon$MayPValue[9] <- summary(Fit.OneOut.Burglary.Mon)$coef[17,4]
Summary.Table.OneOut.Mon$MayPValue[10] <- summary(Fit.OneOut.Homicide.Mon)$coef[17,4]
Summary.Table.OneOut.Mon$MayPValue[11] <- summary(Fit.OneOut.DeceptivePractice.Mon)$coef[17,4]

Summary.Table.OneOut.Mon$JunPValue[1] <- summary(Fit.OneOut.AllCrimes.Mon)$coef[18,4]
Summary.Table.OneOut.Mon$JunPValue[2] <- summary(Fit.OneOut.Thefts.Mon)$coef[18,4]
Summary.Table.OneOut.Mon$JunPValue[3] <- summary(Fit.OneOut.Narcotics.Mon)$coef[18,4]
Summary.Table.OneOut.Mon$JunPValue[4] <- summary(Fit.OneOut.Battery.Mon)$coef[18,4]
Summary.Table.OneOut.Mon$JunPValue[5] <- summary(Fit.OneOut.CriminalDamage.Mon)$coef[18,4]
Summary.Table.OneOut.Mon$JunPValue[6] <- summary(Fit.OneOut.MotorVehicleTheft.Mon)$coef[18,4]
Summary.Table.OneOut.Mon$JunPValue[7] <- summary(Fit.OneOut.Robbery.Mon)$coef[18,4]
Summary.Table.OneOut.Mon$JunPValue[8] <- summary(Fit.OneOut.Assault.Mon)$coef[18,4]
Summary.Table.OneOut.Mon$JunPValue[9] <- summary(Fit.OneOut.Burglary.Mon)$coef[18,4]
Summary.Table.OneOut.Mon$JunPValue[10] <- summary(Fit.OneOut.Homicide.Mon)$coef[18,4]
Summary.Table.OneOut.Mon$JunPValue[11] <- summary(Fit.OneOut.DeceptivePractice.Mon)$coef[18,4]

Summary.Table.OneOut.Mon$JulPValue[1] <- summary(Fit.OneOut.AllCrimes.Mon)$coef[19,4]
Summary.Table.OneOut.Mon$JulPValue[2] <- summary(Fit.OneOut.Thefts.Mon)$coef[19,4]
Summary.Table.OneOut.Mon$JulPValue[3] <- summary(Fit.OneOut.Narcotics.Mon)$coef[19,4]
Summary.Table.OneOut.Mon$JulPValue[4] <- summary(Fit.OneOut.Battery.Mon)$coef[19,4]
Summary.Table.OneOut.Mon$JulPValue[5] <- summary(Fit.OneOut.CriminalDamage.Mon)$coef[19,4]
Summary.Table.OneOut.Mon$JulPValue[6] <- summary(Fit.OneOut.MotorVehicleTheft.Mon)$coef[19,4]
Summary.Table.OneOut.Mon$JulPValue[7] <- summary(Fit.OneOut.Robbery.Mon)$coef[19,4]
Summary.Table.OneOut.Mon$JulPValue[8] <- summary(Fit.OneOut.Assault.Mon)$coef[19,4]
Summary.Table.OneOut.Mon$JulPValue[9] <- summary(Fit.OneOut.Burglary.Mon)$coef[19,4]
Summary.Table.OneOut.Mon$JulPValue[10] <- summary(Fit.OneOut.Homicide.Mon)$coef[19,4]
Summary.Table.OneOut.Mon$JulPValue[11] <- summary(Fit.OneOut.DeceptivePractice.Mon)$coef[19,4]

Summary.Table.OneOut.Mon$AugPValue[1] <- summary(Fit.OneOut.AllCrimes.Mon)$coef[20,4]
Summary.Table.OneOut.Mon$AugPValue[2] <- summary(Fit.OneOut.Thefts.Mon)$coef[20,4]
Summary.Table.OneOut.Mon$AugPValue[3] <- summary(Fit.OneOut.Narcotics.Mon)$coef[20,4]
Summary.Table.OneOut.Mon$AugPValue[4] <- summary(Fit.OneOut.Battery.Mon)$coef[20,4]
Summary.Table.OneOut.Mon$AugPValue[5] <- summary(Fit.OneOut.CriminalDamage.Mon)$coef[20,4]
Summary.Table.OneOut.Mon$AugPValue[6] <- summary(Fit.OneOut.MotorVehicleTheft.Mon)$coef[20,4]
Summary.Table.OneOut.Mon$AugPValue[7] <- summary(Fit.OneOut.Robbery.Mon)$coef[20,4]
Summary.Table.OneOut.Mon$AugPValue[8] <- summary(Fit.OneOut.Assault.Mon)$coef[20,4]
Summary.Table.OneOut.Mon$AugPValue[9] <- summary(Fit.OneOut.Burglary.Mon)$coef[20,4]
Summary.Table.OneOut.Mon$AugPValue[10] <- summary(Fit.OneOut.Homicide.Mon)$coef[20,4]
Summary.Table.OneOut.Mon$AugPValue[11] <- summary(Fit.OneOut.DeceptivePractice.Mon)$coef[20,4]

Summary.Table.OneOut.Mon$SepPValue[1] <- summary(Fit.OneOut.AllCrimes.Mon)$coef[21,4]
Summary.Table.OneOut.Mon$SepPValue[2] <- summary(Fit.OneOut.Thefts.Mon)$coef[21,4]
Summary.Table.OneOut.Mon$SepPValue[3] <- summary(Fit.OneOut.Narcotics.Mon)$coef[21,4]
Summary.Table.OneOut.Mon$SepPValue[4] <- summary(Fit.OneOut.Battery.Mon)$coef[21,4]
Summary.Table.OneOut.Mon$SepPValue[5] <- summary(Fit.OneOut.CriminalDamage.Mon)$coef[21,4]
Summary.Table.OneOut.Mon$SepPValue[6] <- summary(Fit.OneOut.MotorVehicleTheft.Mon)$coef[21,4]
Summary.Table.OneOut.Mon$SepPValue[7] <- summary(Fit.OneOut.Robbery.Mon)$coef[21,4]
Summary.Table.OneOut.Mon$SepPValue[8] <- summary(Fit.OneOut.Assault.Mon)$coef[21,4]
Summary.Table.OneOut.Mon$SepPValue[9] <- summary(Fit.OneOut.Burglary.Mon)$coef[21,4]
Summary.Table.OneOut.Mon$SepPValue[10] <- summary(Fit.OneOut.Homicide.Mon)$coef[21,4]
Summary.Table.OneOut.Mon$SepPValue[11] <- summary(Fit.OneOut.DeceptivePractice.Mon)$coef[21,4]

Summary.Table.OneOut.Mon$OctPValue[1] <- summary(Fit.OneOut.AllCrimes.Mon)$coef[22,4]
Summary.Table.OneOut.Mon$OctPValue[2] <- summary(Fit.OneOut.Thefts.Mon)$coef[22,4]
Summary.Table.OneOut.Mon$OctPValue[3] <- summary(Fit.OneOut.Narcotics.Mon)$coef[22,4]
Summary.Table.OneOut.Mon$OctPValue[4] <- summary(Fit.OneOut.Battery.Mon)$coef[22,4]
Summary.Table.OneOut.Mon$OctPValue[5] <- summary(Fit.OneOut.CriminalDamage.Mon)$coef[22,4]
Summary.Table.OneOut.Mon$OctPValue[6] <- summary(Fit.OneOut.MotorVehicleTheft.Mon)$coef[22,4]
Summary.Table.OneOut.Mon$OctPValue[7] <- summary(Fit.OneOut.Robbery.Mon)$coef[22,4]
Summary.Table.OneOut.Mon$OctPValue[8] <- summary(Fit.OneOut.Assault.Mon)$coef[22,4]
Summary.Table.OneOut.Mon$OctPValue[9] <- summary(Fit.OneOut.Burglary.Mon)$coef[22,4]
Summary.Table.OneOut.Mon$OctPValue[10] <- summary(Fit.OneOut.Homicide.Mon)$coef[22,4]
Summary.Table.OneOut.Mon$OctPValue[11] <- summary(Fit.OneOut.DeceptivePractice.Mon)$coef[22,4]

Summary.Table.OneOut.Mon$NovPValue[1] <- summary(Fit.OneOut.AllCrimes.Mon)$coef[23,4]
Summary.Table.OneOut.Mon$NovPValue[2] <- summary(Fit.OneOut.Thefts.Mon)$coef[23,4]
Summary.Table.OneOut.Mon$NovPValue[3] <- summary(Fit.OneOut.Narcotics.Mon)$coef[23,4]
Summary.Table.OneOut.Mon$NovPValue[4] <- summary(Fit.OneOut.Battery.Mon)$coef[23,4]
Summary.Table.OneOut.Mon$NovPValue[5] <- summary(Fit.OneOut.CriminalDamage.Mon)$coef[23,4]
Summary.Table.OneOut.Mon$NovPValue[6] <- summary(Fit.OneOut.MotorVehicleTheft.Mon)$coef[23,4]
Summary.Table.OneOut.Mon$NovPValue[7] <- summary(Fit.OneOut.Robbery.Mon)$coef[23,4]
Summary.Table.OneOut.Mon$NovPValue[8] <- summary(Fit.OneOut.Assault.Mon)$coef[23,4]
Summary.Table.OneOut.Mon$NovPValue[9] <- summary(Fit.OneOut.Burglary.Mon)$coef[23,4]
Summary.Table.OneOut.Mon$NovPValue[10] <- summary(Fit.OneOut.Homicide.Mon)$coef[23,4]
Summary.Table.OneOut.Mon$NovPValue[11] <- summary(Fit.OneOut.DeceptivePractice.Mon)$coef[23,4]

Summary.Table.OneOut.Mon$DecPValue[1] <- summary(Fit.OneOut.AllCrimes.Mon)$coef[24,4]
Summary.Table.OneOut.Mon$DecPValue[2] <- summary(Fit.OneOut.Thefts.Mon)$coef[24,4]
Summary.Table.OneOut.Mon$DecPValue[3] <- summary(Fit.OneOut.Narcotics.Mon)$coef[24,4]
Summary.Table.OneOut.Mon$DecPValue[4] <- summary(Fit.OneOut.Battery.Mon)$coef[24,4]
Summary.Table.OneOut.Mon$DecPValue[5] <- summary(Fit.OneOut.CriminalDamage.Mon)$coef[24,4]
Summary.Table.OneOut.Mon$DecPValue[6] <- summary(Fit.OneOut.MotorVehicleTheft.Mon)$coef[24,4]
Summary.Table.OneOut.Mon$DecPValue[7] <- summary(Fit.OneOut.Robbery.Mon)$coef[24,4]
Summary.Table.OneOut.Mon$DecPValue[8] <- summary(Fit.OneOut.Assault.Mon)$coef[24,4]
Summary.Table.OneOut.Mon$DecPValue[9] <- summary(Fit.OneOut.Burglary.Mon)$coef[24,4]
Summary.Table.OneOut.Mon$DecPValue[10] <- summary(Fit.OneOut.Homicide.Mon)$coef[24,4]
Summary.Table.OneOut.Mon$DecPValue[11] <- summary(Fit.OneOut.DeceptivePractice.Mon)$coef[24,4]

Summary.Table.OneOut.Mon$JanStar[which(Summary.Table.OneOut.Mon$JanPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.OneOut.Mon$JanPValue<0.01)))
Summary.Table.OneOut.Mon$JanStar[which(Summary.Table.OneOut.Mon$JanPValue<0.05 & Summary.Table.OneOut.Mon$JanPValue>=0.01)] <- rep("*" , length(which(Summary.Table.OneOut.Mon$JanPValue<0.05 & Summary.Table.OneOut.Mon$JanPValue>=0.01)))
Summary.Table.OneOut.Mon$FebStar[which(Summary.Table.OneOut.Mon$FebPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.OneOut.Mon$FebPValue<0.01)))
Summary.Table.OneOut.Mon$FebStar[which(Summary.Table.OneOut.Mon$FebPValue<0.05 & Summary.Table.OneOut.Mon$FebPValue>=0.01)] <- rep("*" , length(which(Summary.Table.OneOut.Mon$FebPValue<0.05 & Summary.Table.OneOut.Mon$FebPValue>=0.01)))
Summary.Table.OneOut.Mon$MarStar[which(Summary.Table.OneOut.Mon$MarPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.OneOut.Mon$MarPValue<0.01)))
Summary.Table.OneOut.Mon$MarStar[which(Summary.Table.OneOut.Mon$MarPValue<0.05 & Summary.Table.OneOut.Mon$MarPValue>=0.01)] <- rep("*" , length(which(Summary.Table.OneOut.Mon$MarPValue<0.05 & Summary.Table.OneOut.Mon$MarPValue>=0.01)))
Summary.Table.OneOut.Mon$AprStar[which(Summary.Table.OneOut.Mon$AprPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.OneOut.Mon$AprPValue<0.01)))
Summary.Table.OneOut.Mon$AprStar[which(Summary.Table.OneOut.Mon$AprPValue<0.05 & Summary.Table.OneOut.Mon$AprPValue>=0.01)] <- rep("*" , length(which(Summary.Table.OneOut.Mon$AprPValue<0.05 & Summary.Table.OneOut.Mon$AprPValue>=0.01)))
Summary.Table.OneOut.Mon$MayStar[which(Summary.Table.OneOut.Mon$MayPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.OneOut.Mon$MayPValue<0.01)))
Summary.Table.OneOut.Mon$MayStar[which(Summary.Table.OneOut.Mon$MayPValue<0.05 & Summary.Table.OneOut.Mon$MayPValue>=0.01)] <- rep("*" , length(which(Summary.Table.OneOut.Mon$MayPValue<0.05 & Summary.Table.OneOut.Mon$MayPValue>=0.01)))
Summary.Table.OneOut.Mon$JunStar[which(Summary.Table.OneOut.Mon$JunPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.OneOut.Mon$JunPValue<0.01)))
Summary.Table.OneOut.Mon$JunStar[which(Summary.Table.OneOut.Mon$JunPValue<0.05 & Summary.Table.OneOut.Mon$JunPValue>=0.01)] <- rep("*" , length(which(Summary.Table.OneOut.Mon$JunPValue<0.05 & Summary.Table.OneOut.Mon$JunPValue>=0.01)))
Summary.Table.OneOut.Mon$JulStar[which(Summary.Table.OneOut.Mon$JulPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.OneOut.Mon$JulPValue<0.01)))
Summary.Table.OneOut.Mon$JulStar[which(Summary.Table.OneOut.Mon$JulPValue<0.05 & Summary.Table.OneOut.Mon$JulPValue>=0.01)] <- rep("*" , length(which(Summary.Table.OneOut.Mon$JulPValue<0.05 & Summary.Table.OneOut.Mon$JulPValue>=0.01)))
Summary.Table.OneOut.Mon$AugStar[which(Summary.Table.OneOut.Mon$AugPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.OneOut.Mon$AugPValue<0.01)))
Summary.Table.OneOut.Mon$AugStar[which(Summary.Table.OneOut.Mon$AugPValue<0.05 & Summary.Table.OneOut.Mon$AugPValue>=0.01)] <- rep("*" , length(which(Summary.Table.OneOut.Mon$AugPValue<0.05 & Summary.Table.OneOut.Mon$AugPValue>=0.01)))
Summary.Table.OneOut.Mon$SepStar[which(Summary.Table.OneOut.Mon$SepPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.OneOut.Mon$SepPValue<0.01)))
Summary.Table.OneOut.Mon$SepStar[which(Summary.Table.OneOut.Mon$SepPValue<0.05 & Summary.Table.OneOut.Mon$SepPValue>=0.01)] <- rep("*" , length(which(Summary.Table.OneOut.Mon$SepPValue<0.05 & Summary.Table.OneOut.Mon$SepPValue>=0.01)))
Summary.Table.OneOut.Mon$OctStar[which(Summary.Table.OneOut.Mon$OctPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.OneOut.Mon$OctPValue<0.01)))
Summary.Table.OneOut.Mon$OctStar[which(Summary.Table.OneOut.Mon$OctPValue<0.05 & Summary.Table.OneOut.Mon$OctPValue>=0.01)] <- rep("*" , length(which(Summary.Table.OneOut.Mon$OctPValue<0.05 & Summary.Table.OneOut.Mon$OctPValue>=0.01)))
Summary.Table.OneOut.Mon$NovStar[which(Summary.Table.OneOut.Mon$NovPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.OneOut.Mon$NovPValue<0.01)))
Summary.Table.OneOut.Mon$NovStar[which(Summary.Table.OneOut.Mon$NovPValue<0.05 & Summary.Table.OneOut.Mon$NovPValue>=0.01)] <- rep("*" , length(which(Summary.Table.OneOut.Mon$NovPValue<0.05 & Summary.Table.OneOut.Mon$NovPValue>=0.01)))
Summary.Table.OneOut.Mon$DecStar[which(Summary.Table.OneOut.Mon$DecPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.OneOut.Mon$DecPValue<0.01)))
Summary.Table.OneOut.Mon$DecStar[which(Summary.Table.OneOut.Mon$DecPValue<0.05 & Summary.Table.OneOut.Mon$DecPValue>=0.01)] <- rep("*" , length(which(Summary.Table.OneOut.Mon$DecPValue<0.05 & Summary.Table.OneOut.Mon$DecPValue>=0.01)))




Street.Lights.AllOut.Pois$JanInd <- Street.Lights.AllOut.Pois$Jan * Street.Lights.AllOut.Pois$OutageInd
Street.Lights.AllOut.Pois$FebInd <- Street.Lights.AllOut.Pois$Feb * Street.Lights.AllOut.Pois$OutageInd
Street.Lights.AllOut.Pois$MarInd <- Street.Lights.AllOut.Pois$Mar * Street.Lights.AllOut.Pois$OutageInd
Street.Lights.AllOut.Pois$AprInd <- Street.Lights.AllOut.Pois$Apr * Street.Lights.AllOut.Pois$OutageInd
Street.Lights.AllOut.Pois$MayInd <- Street.Lights.AllOut.Pois$May * Street.Lights.AllOut.Pois$OutageInd
Street.Lights.AllOut.Pois$JunInd <- Street.Lights.AllOut.Pois$Jun * Street.Lights.AllOut.Pois$OutageInd
Street.Lights.AllOut.Pois$JulInd <- Street.Lights.AllOut.Pois$Jul * Street.Lights.AllOut.Pois$OutageInd
Street.Lights.AllOut.Pois$AugInd <- Street.Lights.AllOut.Pois$Aug * Street.Lights.AllOut.Pois$OutageInd
Street.Lights.AllOut.Pois$SepInd <- Street.Lights.AllOut.Pois$Sep * Street.Lights.AllOut.Pois$OutageInd
Street.Lights.AllOut.Pois$OctInd <- Street.Lights.AllOut.Pois$Oct * Street.Lights.AllOut.Pois$OutageInd
Street.Lights.AllOut.Pois$NovInd <- Street.Lights.AllOut.Pois$Nov * Street.Lights.AllOut.Pois$OutageInd
Street.Lights.AllOut.Pois$DecInd <- Street.Lights.AllOut.Pois$Dec * Street.Lights.AllOut.Pois$OutageInd

Summary.Table.AllOut.Mon  <- data.frame(matrix(ncol = 36, nrow = 11), row.names = c("All Crimes (No Deceptive Practice)", 
                                                                                    "Thefts", "Narcotics", "Battery", "Criminal Damage", "Motor Vehicle Theft", 
                                                                                    "Robbery", "Assault", "Burglary", "Homicide", "Deceptive Practice"))

Summary.Table.AllOut.Mon  <- rename(Summary.Table.AllOut.Mon,      c("X1"="JanEffect",  "X2"="JanPValue",  "X3"="JanStar", 
                                                                     "X4"="FebEffect",  "X5"="FebPValue",  "X6"="FebStar",
                                                                     "X7"="MarEffect",  "X8"="MarPValue",  "X9"="MarStar",
                                                                     "X10"="AprEffect", "X11"="AprPValue", "X12"="AprStar",
                                                                     "X13"="MayEffect", "X14"="MayPValue", "X15"="MayStar",
                                                                     "X16"="JunEffect", "X17"="JunPValue", "X18"="JunStar",
                                                                     "X19"="JulEffect", "X20"="JulPValue", "X21"="JulStar",
                                                                     "X22"="AugEffect", "X23"="AugPValue", "X24"="AugStar",
                                                                     "X25"="SepEffect", "X26"="SepPValue", "X27"="SepStar",
                                                                     "X28"="OctEffect", "X29"="OctPValue", "X30"="OctStar",
                                                                     "X31"="NovEffect", "X32"="NovPValue", "X33"="NovStar",
                                                                     "X34"="DecEffect", "X35"="DecPValue", "X36"="DecStar"))



Fit.AllOut.AllCrimes.Mon         <- glm(AllCrimes         ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Thefts.Mon            <- glm(Thefts            ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Narcotics.Mon         <- glm(Narcotics         ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Battery.Mon           <- glm(Battery           ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.CriminalDamage.Mon    <- glm(CriminalDamage    ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.MotorVehicleTheft.Mon <- glm(MotorVehicleTheft ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Robbery.Mon           <- glm(Robbery           ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Assault.Mon           <- glm(Assault           ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Burglary.Mon          <- glm(Burglary          ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.Homicide.Mon          <- glm(Homicide          ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)
Fit.AllOut.DeceptivePractice.Mon <- glm(DeceptivePractice ~ offset(log(Duration)) + Feb + Mar + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + JanInd + FebInd + MarInd + AprInd + MayInd + JunInd + JulInd + AugInd + SepInd + OctInd + NovInd + DecInd, family=poisson(link=log), data=Street.Lights.AllOut.Pois)

Summary.Table.AllOut.Mon$JanStar <- character(nrow(Summary.Table.AllOut.Mon))
Summary.Table.AllOut.Mon$FebStar <- character(nrow(Summary.Table.AllOut.Mon))
Summary.Table.AllOut.Mon$MarStar <- character(nrow(Summary.Table.AllOut.Mon))
Summary.Table.AllOut.Mon$AprStar <- character(nrow(Summary.Table.AllOut.Mon))
Summary.Table.AllOut.Mon$MayStar <- character(nrow(Summary.Table.AllOut.Mon))
Summary.Table.AllOut.Mon$JunStar <- character(nrow(Summary.Table.AllOut.Mon))
Summary.Table.AllOut.Mon$JulStar <- character(nrow(Summary.Table.AllOut.Mon))
Summary.Table.AllOut.Mon$AugStar <- character(nrow(Summary.Table.AllOut.Mon))
Summary.Table.AllOut.Mon$SepStar <- character(nrow(Summary.Table.AllOut.Mon))
Summary.Table.AllOut.Mon$OctStar <- character(nrow(Summary.Table.AllOut.Mon))
Summary.Table.AllOut.Mon$NovStar <- character(nrow(Summary.Table.AllOut.Mon))
Summary.Table.AllOut.Mon$DecStar <- character(nrow(Summary.Table.AllOut.Mon))

Summary.Table.AllOut.Mon$JanEffect[1] <- 100*exp(Fit.AllOut.AllCrimes.Mon$coef[13])-100
Summary.Table.AllOut.Mon$JanEffect[2] <- 100*exp(Fit.AllOut.Thefts.Mon$coef[13])-100
Summary.Table.AllOut.Mon$JanEffect[3] <- 100*exp(Fit.AllOut.Narcotics.Mon$coef[13])-100
Summary.Table.AllOut.Mon$JanEffect[4] <- 100*exp(Fit.AllOut.Battery.Mon$coef[13])-100
Summary.Table.AllOut.Mon$JanEffect[5] <- 100*exp(Fit.AllOut.CriminalDamage.Mon$coef[13])-100
Summary.Table.AllOut.Mon$JanEffect[6] <- 100*exp(Fit.AllOut.MotorVehicleTheft.Mon$coef[13])-100
Summary.Table.AllOut.Mon$JanEffect[7] <- 100*exp(Fit.AllOut.Robbery.Mon$coef[13])-100
Summary.Table.AllOut.Mon$JanEffect[8] <- 100*exp(Fit.AllOut.Assault.Mon$coef[13])-100
Summary.Table.AllOut.Mon$JanEffect[9] <- 100*exp(Fit.AllOut.Burglary.Mon$coef[13])-100
Summary.Table.AllOut.Mon$JanEffect[10] <- 100*exp(Fit.AllOut.Homicide.Mon$coef[13])-100
Summary.Table.AllOut.Mon$JanEffect[11] <- 100*exp(Fit.AllOut.DeceptivePractice.Mon$coef[13])-100

Summary.Table.AllOut.Mon$FebEffect[1] <- 100*exp(Fit.AllOut.AllCrimes.Mon$coef[14])-100
Summary.Table.AllOut.Mon$FebEffect[2] <- 100*exp(Fit.AllOut.Thefts.Mon$coef[14])-100
Summary.Table.AllOut.Mon$FebEffect[3] <- 100*exp(Fit.AllOut.Narcotics.Mon$coef[14])-100
Summary.Table.AllOut.Mon$FebEffect[4] <- 100*exp(Fit.AllOut.Battery.Mon$coef[14])-100
Summary.Table.AllOut.Mon$FebEffect[5] <- 100*exp(Fit.AllOut.CriminalDamage.Mon$coef[14])-100
Summary.Table.AllOut.Mon$FebEffect[6] <- 100*exp(Fit.AllOut.MotorVehicleTheft.Mon$coef[14])-100
Summary.Table.AllOut.Mon$FebEffect[7] <- 100*exp(Fit.AllOut.Robbery.Mon$coef[14])-100
Summary.Table.AllOut.Mon$FebEffect[8] <- 100*exp(Fit.AllOut.Assault.Mon$coef[14])-100
Summary.Table.AllOut.Mon$FebEffect[9] <- 100*exp(Fit.AllOut.Burglary.Mon$coef[14])-100
Summary.Table.AllOut.Mon$FebEffect[10] <- 100*exp(Fit.AllOut.Homicide.Mon$coef[14])-100
Summary.Table.AllOut.Mon$FebEffect[11] <- 100*exp(Fit.AllOut.DeceptivePractice.Mon$coef[14])-100

Summary.Table.AllOut.Mon$MarEffect[1] <- 100*exp(Fit.AllOut.AllCrimes.Mon$coef[15])-100
Summary.Table.AllOut.Mon$MarEffect[2] <- 100*exp(Fit.AllOut.Thefts.Mon$coef[15])-100
Summary.Table.AllOut.Mon$MarEffect[3] <- 100*exp(Fit.AllOut.Narcotics.Mon$coef[15])-100
Summary.Table.AllOut.Mon$MarEffect[4] <- 100*exp(Fit.AllOut.Battery.Mon$coef[15])-100
Summary.Table.AllOut.Mon$MarEffect[5] <- 100*exp(Fit.AllOut.CriminalDamage.Mon$coef[15])-100
Summary.Table.AllOut.Mon$MarEffect[6] <- 100*exp(Fit.AllOut.MotorVehicleTheft.Mon$coef[15])-100
Summary.Table.AllOut.Mon$MarEffect[7] <- 100*exp(Fit.AllOut.Robbery.Mon$coef[15])-100
Summary.Table.AllOut.Mon$MarEffect[8] <- 100*exp(Fit.AllOut.Assault.Mon$coef[15])-100
Summary.Table.AllOut.Mon$MarEffect[9] <- 100*exp(Fit.AllOut.Burglary.Mon$coef[15])-100
Summary.Table.AllOut.Mon$MarEffect[10] <- 100*exp(Fit.AllOut.Homicide.Mon$coef[15])-100
Summary.Table.AllOut.Mon$MarEffect[11] <- 100*exp(Fit.AllOut.DeceptivePractice.Mon$coef[15])-100

Summary.Table.AllOut.Mon$AprEffect[1] <- 100*exp(Fit.AllOut.AllCrimes.Mon$coef[16])-100
Summary.Table.AllOut.Mon$AprEffect[2] <- 100*exp(Fit.AllOut.Thefts.Mon$coef[16])-100
Summary.Table.AllOut.Mon$AprEffect[3] <- 100*exp(Fit.AllOut.Narcotics.Mon$coef[16])-100
Summary.Table.AllOut.Mon$AprEffect[4] <- 100*exp(Fit.AllOut.Battery.Mon$coef[16])-100
Summary.Table.AllOut.Mon$AprEffect[5] <- 100*exp(Fit.AllOut.CriminalDamage.Mon$coef[16])-100
Summary.Table.AllOut.Mon$AprEffect[6] <- 100*exp(Fit.AllOut.MotorVehicleTheft.Mon$coef[16])-100
Summary.Table.AllOut.Mon$AprEffect[7] <- 100*exp(Fit.AllOut.Robbery.Mon$coef[16])-100
Summary.Table.AllOut.Mon$AprEffect[8] <- 100*exp(Fit.AllOut.Assault.Mon$coef[16])-100
Summary.Table.AllOut.Mon$AprEffect[9] <- 100*exp(Fit.AllOut.Burglary.Mon$coef[16])-100
Summary.Table.AllOut.Mon$AprEffect[10] <- 100*exp(Fit.AllOut.Homicide.Mon$coef[16])-100
Summary.Table.AllOut.Mon$AprEffect[11] <- 100*exp(Fit.AllOut.DeceptivePractice.Mon$coef[16])-100

Summary.Table.AllOut.Mon$MayEffect[1] <- 100*exp(Fit.AllOut.AllCrimes.Mon$coef[17])-100
Summary.Table.AllOut.Mon$MayEffect[2] <- 100*exp(Fit.AllOut.Thefts.Mon$coef[17])-100
Summary.Table.AllOut.Mon$MayEffect[3] <- 100*exp(Fit.AllOut.Narcotics.Mon$coef[17])-100
Summary.Table.AllOut.Mon$MayEffect[4] <- 100*exp(Fit.AllOut.Battery.Mon$coef[17])-100
Summary.Table.AllOut.Mon$MayEffect[5] <- 100*exp(Fit.AllOut.CriminalDamage.Mon$coef[17])-100
Summary.Table.AllOut.Mon$MayEffect[6] <- 100*exp(Fit.AllOut.MotorVehicleTheft.Mon$coef[17])-100
Summary.Table.AllOut.Mon$MayEffect[7] <- 100*exp(Fit.AllOut.Robbery.Mon$coef[17])-100
Summary.Table.AllOut.Mon$MayEffect[8] <- 100*exp(Fit.AllOut.Assault.Mon$coef[17])-100
Summary.Table.AllOut.Mon$MayEffect[9] <- 100*exp(Fit.AllOut.Burglary.Mon$coef[17])-100
Summary.Table.AllOut.Mon$MayEffect[10] <- 100*exp(Fit.AllOut.Homicide.Mon$coef[17])-100
Summary.Table.AllOut.Mon$MayEffect[11] <- 100*exp(Fit.AllOut.DeceptivePractice.Mon$coef[17])-100

Summary.Table.AllOut.Mon$JunEffect[1] <- 100*exp(Fit.AllOut.AllCrimes.Mon$coef[18])-100
Summary.Table.AllOut.Mon$JunEffect[2] <- 100*exp(Fit.AllOut.Thefts.Mon$coef[18])-100
Summary.Table.AllOut.Mon$JunEffect[3] <- 100*exp(Fit.AllOut.Narcotics.Mon$coef[18])-100
Summary.Table.AllOut.Mon$JunEffect[4] <- 100*exp(Fit.AllOut.Battery.Mon$coef[18])-100
Summary.Table.AllOut.Mon$JunEffect[5] <- 100*exp(Fit.AllOut.CriminalDamage.Mon$coef[18])-100
Summary.Table.AllOut.Mon$JunEffect[6] <- 100*exp(Fit.AllOut.MotorVehicleTheft.Mon$coef[18])-100
Summary.Table.AllOut.Mon$JunEffect[7] <- 100*exp(Fit.AllOut.Robbery.Mon$coef[18])-100
Summary.Table.AllOut.Mon$JunEffect[8] <- 100*exp(Fit.AllOut.Assault.Mon$coef[18])-100
Summary.Table.AllOut.Mon$JunEffect[9] <- 100*exp(Fit.AllOut.Burglary.Mon$coef[18])-100
Summary.Table.AllOut.Mon$JunEffect[10] <- 100*exp(Fit.AllOut.Homicide.Mon$coef[18])-100
Summary.Table.AllOut.Mon$JunEffect[11] <- 100*exp(Fit.AllOut.DeceptivePractice.Mon$coef[18])-100

Summary.Table.AllOut.Mon$JulEffect[1] <- 100*exp(Fit.AllOut.AllCrimes.Mon$coef[19])-100
Summary.Table.AllOut.Mon$JulEffect[2] <- 100*exp(Fit.AllOut.Thefts.Mon$coef[19])-100
Summary.Table.AllOut.Mon$JulEffect[3] <- 100*exp(Fit.AllOut.Narcotics.Mon$coef[19])-100
Summary.Table.AllOut.Mon$JulEffect[4] <- 100*exp(Fit.AllOut.Battery.Mon$coef[19])-100
Summary.Table.AllOut.Mon$JulEffect[5] <- 100*exp(Fit.AllOut.CriminalDamage.Mon$coef[19])-100
Summary.Table.AllOut.Mon$JulEffect[6] <- 100*exp(Fit.AllOut.MotorVehicleTheft.Mon$coef[19])-100
Summary.Table.AllOut.Mon$JulEffect[7] <- 100*exp(Fit.AllOut.Robbery.Mon$coef[19])-100
Summary.Table.AllOut.Mon$JulEffect[8] <- 100*exp(Fit.AllOut.Assault.Mon$coef[19])-100
Summary.Table.AllOut.Mon$JulEffect[9] <- 100*exp(Fit.AllOut.Burglary.Mon$coef[19])-100
Summary.Table.AllOut.Mon$JulEffect[10] <- 100*exp(Fit.AllOut.Homicide.Mon$coef[19])-100
Summary.Table.AllOut.Mon$JulEffect[11] <- 100*exp(Fit.AllOut.DeceptivePractice.Mon$coef[19])-100

Summary.Table.AllOut.Mon$AugEffect[1] <- 100*exp(Fit.AllOut.AllCrimes.Mon$coef[20])-100
Summary.Table.AllOut.Mon$AugEffect[2] <- 100*exp(Fit.AllOut.Thefts.Mon$coef[20])-100
Summary.Table.AllOut.Mon$AugEffect[3] <- 100*exp(Fit.AllOut.Narcotics.Mon$coef[20])-100
Summary.Table.AllOut.Mon$AugEffect[4] <- 100*exp(Fit.AllOut.Battery.Mon$coef[20])-100
Summary.Table.AllOut.Mon$AugEffect[5] <- 100*exp(Fit.AllOut.CriminalDamage.Mon$coef[20])-100
Summary.Table.AllOut.Mon$AugEffect[6] <- 100*exp(Fit.AllOut.MotorVehicleTheft.Mon$coef[20])-100
Summary.Table.AllOut.Mon$AugEffect[7] <- 100*exp(Fit.AllOut.Robbery.Mon$coef[20])-100
Summary.Table.AllOut.Mon$AugEffect[8] <- 100*exp(Fit.AllOut.Assault.Mon$coef[20])-100
Summary.Table.AllOut.Mon$AugEffect[9] <- 100*exp(Fit.AllOut.Burglary.Mon$coef[20])-100
Summary.Table.AllOut.Mon$AugEffect[10] <- 100*exp(Fit.AllOut.Homicide.Mon$coef[20])-100
Summary.Table.AllOut.Mon$AugEffect[11] <- 100*exp(Fit.AllOut.DeceptivePractice.Mon$coef[20])-100

Summary.Table.AllOut.Mon$SepEffect[1] <- 100*exp(Fit.AllOut.AllCrimes.Mon$coef[21])-100
Summary.Table.AllOut.Mon$SepEffect[2] <- 100*exp(Fit.AllOut.Thefts.Mon$coef[21])-100
Summary.Table.AllOut.Mon$SepEffect[3] <- 100*exp(Fit.AllOut.Narcotics.Mon$coef[21])-100
Summary.Table.AllOut.Mon$SepEffect[4] <- 100*exp(Fit.AllOut.Battery.Mon$coef[21])-100
Summary.Table.AllOut.Mon$SepEffect[5] <- 100*exp(Fit.AllOut.CriminalDamage.Mon$coef[21])-100
Summary.Table.AllOut.Mon$SepEffect[6] <- 100*exp(Fit.AllOut.MotorVehicleTheft.Mon$coef[21])-100
Summary.Table.AllOut.Mon$SepEffect[7] <- 100*exp(Fit.AllOut.Robbery.Mon$coef[21])-100
Summary.Table.AllOut.Mon$SepEffect[8] <- 100*exp(Fit.AllOut.Assault.Mon$coef[21])-100
Summary.Table.AllOut.Mon$SepEffect[9] <- 100*exp(Fit.AllOut.Burglary.Mon$coef[21])-100
Summary.Table.AllOut.Mon$SepEffect[10] <- 100*exp(Fit.AllOut.Homicide.Mon$coef[21])-100
Summary.Table.AllOut.Mon$SepEffect[11] <- 100*exp(Fit.AllOut.DeceptivePractice.Mon$coef[21])-100

Summary.Table.AllOut.Mon$OctEffect[1] <- 100*exp(Fit.AllOut.AllCrimes.Mon$coef[22])-100
Summary.Table.AllOut.Mon$OctEffect[2] <- 100*exp(Fit.AllOut.Thefts.Mon$coef[22])-100
Summary.Table.AllOut.Mon$OctEffect[3] <- 100*exp(Fit.AllOut.Narcotics.Mon$coef[22])-100
Summary.Table.AllOut.Mon$OctEffect[4] <- 100*exp(Fit.AllOut.Battery.Mon$coef[22])-100
Summary.Table.AllOut.Mon$OctEffect[5] <- 100*exp(Fit.AllOut.CriminalDamage.Mon$coef[22])-100
Summary.Table.AllOut.Mon$OctEffect[6] <- 100*exp(Fit.AllOut.MotorVehicleTheft.Mon$coef[22])-100
Summary.Table.AllOut.Mon$OctEffect[7] <- 100*exp(Fit.AllOut.Robbery.Mon$coef[22])-100
Summary.Table.AllOut.Mon$OctEffect[8] <- 100*exp(Fit.AllOut.Assault.Mon$coef[22])-100
Summary.Table.AllOut.Mon$OctEffect[9] <- 100*exp(Fit.AllOut.Burglary.Mon$coef[22])-100
Summary.Table.AllOut.Mon$OctEffect[10] <- 100*exp(Fit.AllOut.Homicide.Mon$coef[22])-100
Summary.Table.AllOut.Mon$OctEffect[11] <- 100*exp(Fit.AllOut.DeceptivePractice.Mon$coef[22])-100

Summary.Table.AllOut.Mon$NovEffect[1] <- 100*exp(Fit.AllOut.AllCrimes.Mon$coef[23])-100
Summary.Table.AllOut.Mon$NovEffect[2] <- 100*exp(Fit.AllOut.Thefts.Mon$coef[23])-100
Summary.Table.AllOut.Mon$NovEffect[3] <- 100*exp(Fit.AllOut.Narcotics.Mon$coef[23])-100
Summary.Table.AllOut.Mon$NovEffect[4] <- 100*exp(Fit.AllOut.Battery.Mon$coef[23])-100
Summary.Table.AllOut.Mon$NovEffect[5] <- 100*exp(Fit.AllOut.CriminalDamage.Mon$coef[23])-100
Summary.Table.AllOut.Mon$NovEffect[6] <- 100*exp(Fit.AllOut.MotorVehicleTheft.Mon$coef[23])-100
Summary.Table.AllOut.Mon$NovEffect[7] <- 100*exp(Fit.AllOut.Robbery.Mon$coef[23])-100
Summary.Table.AllOut.Mon$NovEffect[8] <- 100*exp(Fit.AllOut.Assault.Mon$coef[23])-100
Summary.Table.AllOut.Mon$NovEffect[9] <- 100*exp(Fit.AllOut.Burglary.Mon$coef[23])-100
Summary.Table.AllOut.Mon$NovEffect[10] <- 100*exp(Fit.AllOut.Homicide.Mon$coef[23])-100
Summary.Table.AllOut.Mon$NovEffect[11] <- 100*exp(Fit.AllOut.DeceptivePractice.Mon$coef[23])-100

Summary.Table.AllOut.Mon$DecEffect[1] <- 100*exp(Fit.AllOut.AllCrimes.Mon$coef[24])-100
Summary.Table.AllOut.Mon$DecEffect[2] <- 100*exp(Fit.AllOut.Thefts.Mon$coef[24])-100
Summary.Table.AllOut.Mon$DecEffect[3] <- 100*exp(Fit.AllOut.Narcotics.Mon$coef[24])-100
Summary.Table.AllOut.Mon$DecEffect[4] <- 100*exp(Fit.AllOut.Battery.Mon$coef[24])-100
Summary.Table.AllOut.Mon$DecEffect[5] <- 100*exp(Fit.AllOut.CriminalDamage.Mon$coef[24])-100
Summary.Table.AllOut.Mon$DecEffect[6] <- 100*exp(Fit.AllOut.MotorVehicleTheft.Mon$coef[24])-100
Summary.Table.AllOut.Mon$DecEffect[7] <- 100*exp(Fit.AllOut.Robbery.Mon$coef[24])-100
Summary.Table.AllOut.Mon$DecEffect[8] <- 100*exp(Fit.AllOut.Assault.Mon$coef[24])-100
Summary.Table.AllOut.Mon$DecEffect[9] <- 100*exp(Fit.AllOut.Burglary.Mon$coef[24])-100
Summary.Table.AllOut.Mon$DecEffect[10] <- 100*exp(Fit.AllOut.Homicide.Mon$coef[24])-100
Summary.Table.AllOut.Mon$DecEffect[11] <- 100*exp(Fit.AllOut.DeceptivePractice.Mon$coef[24])-100


Summary.Table.AllOut.Mon$JanPValue[1] <- summary(Fit.AllOut.AllCrimes.Mon)$coef[13,4]
Summary.Table.AllOut.Mon$JanPValue[2] <- summary(Fit.AllOut.Thefts.Mon)$coef[13,4]
Summary.Table.AllOut.Mon$JanPValue[3] <- summary(Fit.AllOut.Narcotics.Mon)$coef[13,4]
Summary.Table.AllOut.Mon$JanPValue[4] <- summary(Fit.AllOut.Battery.Mon)$coef[13,4]
Summary.Table.AllOut.Mon$JanPValue[5] <- summary(Fit.AllOut.CriminalDamage.Mon)$coef[13,4]
Summary.Table.AllOut.Mon$JanPValue[6] <- summary(Fit.AllOut.MotorVehicleTheft.Mon)$coef[13,4]
Summary.Table.AllOut.Mon$JanPValue[7] <- summary(Fit.AllOut.Robbery.Mon)$coef[13,4]
Summary.Table.AllOut.Mon$JanPValue[8] <- summary(Fit.AllOut.Assault.Mon)$coef[13,4]
Summary.Table.AllOut.Mon$JanPValue[9] <- summary(Fit.AllOut.Burglary.Mon)$coef[13,4]
Summary.Table.AllOut.Mon$JanPValue[10] <- summary(Fit.AllOut.Homicide.Mon)$coef[13,4]
Summary.Table.AllOut.Mon$JanPValue[11] <- summary(Fit.AllOut.DeceptivePractice.Mon)$coef[13,4]

Summary.Table.AllOut.Mon$FebPValue[1] <- summary(Fit.AllOut.AllCrimes.Mon)$coef[14,4]
Summary.Table.AllOut.Mon$FebPValue[2] <- summary(Fit.AllOut.Thefts.Mon)$coef[14,4]
Summary.Table.AllOut.Mon$FebPValue[3] <- summary(Fit.AllOut.Narcotics.Mon)$coef[14,4]
Summary.Table.AllOut.Mon$FebPValue[4] <- summary(Fit.AllOut.Battery.Mon)$coef[14,4]
Summary.Table.AllOut.Mon$FebPValue[5] <- summary(Fit.AllOut.CriminalDamage.Mon)$coef[14,4]
Summary.Table.AllOut.Mon$FebPValue[6] <- summary(Fit.AllOut.MotorVehicleTheft.Mon)$coef[14,4]
Summary.Table.AllOut.Mon$FebPValue[7] <- summary(Fit.AllOut.Robbery.Mon)$coef[14,4]
Summary.Table.AllOut.Mon$FebPValue[8] <- summary(Fit.AllOut.Assault.Mon)$coef[14,4]
Summary.Table.AllOut.Mon$FebPValue[9] <- summary(Fit.AllOut.Burglary.Mon)$coef[14,4]
Summary.Table.AllOut.Mon$FebPValue[10] <- summary(Fit.AllOut.Homicide.Mon)$coef[14,4]
Summary.Table.AllOut.Mon$FebPValue[11] <- summary(Fit.AllOut.DeceptivePractice.Mon)$coef[14,4]

Summary.Table.AllOut.Mon$MarPValue[1] <- summary(Fit.AllOut.AllCrimes.Mon)$coef[15,4]
Summary.Table.AllOut.Mon$MarPValue[2] <- summary(Fit.AllOut.Thefts.Mon)$coef[15,4]
Summary.Table.AllOut.Mon$MarPValue[3] <- summary(Fit.AllOut.Narcotics.Mon)$coef[15,4]
Summary.Table.AllOut.Mon$MarPValue[4] <- summary(Fit.AllOut.Battery.Mon)$coef[15,4]
Summary.Table.AllOut.Mon$MarPValue[5] <- summary(Fit.AllOut.CriminalDamage.Mon)$coef[15,4]
Summary.Table.AllOut.Mon$MarPValue[6] <- summary(Fit.AllOut.MotorVehicleTheft.Mon)$coef[15,4]
Summary.Table.AllOut.Mon$MarPValue[7] <- summary(Fit.AllOut.Robbery.Mon)$coef[15,4]
Summary.Table.AllOut.Mon$MarPValue[8] <- summary(Fit.AllOut.Assault.Mon)$coef[15,4]
Summary.Table.AllOut.Mon$MarPValue[9] <- summary(Fit.AllOut.Burglary.Mon)$coef[15,4]
Summary.Table.AllOut.Mon$MarPValue[10] <- summary(Fit.AllOut.Homicide.Mon)$coef[15,4]
Summary.Table.AllOut.Mon$MarPValue[11] <- summary(Fit.AllOut.DeceptivePractice.Mon)$coef[15,4]

Summary.Table.AllOut.Mon$AprPValue[1] <- summary(Fit.AllOut.AllCrimes.Mon)$coef[16,4]
Summary.Table.AllOut.Mon$AprPValue[2] <- summary(Fit.AllOut.Thefts.Mon)$coef[16,4]
Summary.Table.AllOut.Mon$AprPValue[3] <- summary(Fit.AllOut.Narcotics.Mon)$coef[16,4]
Summary.Table.AllOut.Mon$AprPValue[4] <- summary(Fit.AllOut.Battery.Mon)$coef[16,4]
Summary.Table.AllOut.Mon$AprPValue[5] <- summary(Fit.AllOut.CriminalDamage.Mon)$coef[16,4]
Summary.Table.AllOut.Mon$AprPValue[6] <- summary(Fit.AllOut.MotorVehicleTheft.Mon)$coef[16,4]
Summary.Table.AllOut.Mon$AprPValue[7] <- summary(Fit.AllOut.Robbery.Mon)$coef[16,4]
Summary.Table.AllOut.Mon$AprPValue[8] <- summary(Fit.AllOut.Assault.Mon)$coef[16,4]
Summary.Table.AllOut.Mon$AprPValue[9] <- summary(Fit.AllOut.Burglary.Mon)$coef[16,4]
Summary.Table.AllOut.Mon$AprPValue[10] <- summary(Fit.AllOut.Homicide.Mon)$coef[16,4]
Summary.Table.AllOut.Mon$AprPValue[11] <- summary(Fit.AllOut.DeceptivePractice.Mon)$coef[16,4]

Summary.Table.AllOut.Mon$MayPValue[1] <- summary(Fit.AllOut.AllCrimes.Mon)$coef[17,4]
Summary.Table.AllOut.Mon$MayPValue[2] <- summary(Fit.AllOut.Thefts.Mon)$coef[17,4]
Summary.Table.AllOut.Mon$MayPValue[3] <- summary(Fit.AllOut.Narcotics.Mon)$coef[17,4]
Summary.Table.AllOut.Mon$MayPValue[4] <- summary(Fit.AllOut.Battery.Mon)$coef[17,4]
Summary.Table.AllOut.Mon$MayPValue[5] <- summary(Fit.AllOut.CriminalDamage.Mon)$coef[17,4]
Summary.Table.AllOut.Mon$MayPValue[6] <- summary(Fit.AllOut.MotorVehicleTheft.Mon)$coef[17,4]
Summary.Table.AllOut.Mon$MayPValue[7] <- summary(Fit.AllOut.Robbery.Mon)$coef[17,4]
Summary.Table.AllOut.Mon$MayPValue[8] <- summary(Fit.AllOut.Assault.Mon)$coef[17,4]
Summary.Table.AllOut.Mon$MayPValue[9] <- summary(Fit.AllOut.Burglary.Mon)$coef[17,4]
Summary.Table.AllOut.Mon$MayPValue[10] <- summary(Fit.AllOut.Homicide.Mon)$coef[17,4]
Summary.Table.AllOut.Mon$MayPValue[11] <- summary(Fit.AllOut.DeceptivePractice.Mon)$coef[17,4]

Summary.Table.AllOut.Mon$JunPValue[1] <- summary(Fit.AllOut.AllCrimes.Mon)$coef[18,4]
Summary.Table.AllOut.Mon$JunPValue[2] <- summary(Fit.AllOut.Thefts.Mon)$coef[18,4]
Summary.Table.AllOut.Mon$JunPValue[3] <- summary(Fit.AllOut.Narcotics.Mon)$coef[18,4]
Summary.Table.AllOut.Mon$JunPValue[4] <- summary(Fit.AllOut.Battery.Mon)$coef[18,4]
Summary.Table.AllOut.Mon$JunPValue[5] <- summary(Fit.AllOut.CriminalDamage.Mon)$coef[18,4]
Summary.Table.AllOut.Mon$JunPValue[6] <- summary(Fit.AllOut.MotorVehicleTheft.Mon)$coef[18,4]
Summary.Table.AllOut.Mon$JunPValue[7] <- summary(Fit.AllOut.Robbery.Mon)$coef[18,4]
Summary.Table.AllOut.Mon$JunPValue[8] <- summary(Fit.AllOut.Assault.Mon)$coef[18,4]
Summary.Table.AllOut.Mon$JunPValue[9] <- summary(Fit.AllOut.Burglary.Mon)$coef[18,4]
Summary.Table.AllOut.Mon$JunPValue[10] <- summary(Fit.AllOut.Homicide.Mon)$coef[18,4]
Summary.Table.AllOut.Mon$JunPValue[11] <- summary(Fit.AllOut.DeceptivePractice.Mon)$coef[18,4]

Summary.Table.AllOut.Mon$JulPValue[1] <- summary(Fit.AllOut.AllCrimes.Mon)$coef[19,4]
Summary.Table.AllOut.Mon$JulPValue[2] <- summary(Fit.AllOut.Thefts.Mon)$coef[19,4]
Summary.Table.AllOut.Mon$JulPValue[3] <- summary(Fit.AllOut.Narcotics.Mon)$coef[19,4]
Summary.Table.AllOut.Mon$JulPValue[4] <- summary(Fit.AllOut.Battery.Mon)$coef[19,4]
Summary.Table.AllOut.Mon$JulPValue[5] <- summary(Fit.AllOut.CriminalDamage.Mon)$coef[19,4]
Summary.Table.AllOut.Mon$JulPValue[6] <- summary(Fit.AllOut.MotorVehicleTheft.Mon)$coef[19,4]
Summary.Table.AllOut.Mon$JulPValue[7] <- summary(Fit.AllOut.Robbery.Mon)$coef[19,4]
Summary.Table.AllOut.Mon$JulPValue[8] <- summary(Fit.AllOut.Assault.Mon)$coef[19,4]
Summary.Table.AllOut.Mon$JulPValue[9] <- summary(Fit.AllOut.Burglary.Mon)$coef[19,4]
Summary.Table.AllOut.Mon$JulPValue[10] <- summary(Fit.AllOut.Homicide.Mon)$coef[19,4]
Summary.Table.AllOut.Mon$JulPValue[11] <- summary(Fit.AllOut.DeceptivePractice.Mon)$coef[19,4]

Summary.Table.AllOut.Mon$AugPValue[1] <- summary(Fit.AllOut.AllCrimes.Mon)$coef[20,4]
Summary.Table.AllOut.Mon$AugPValue[2] <- summary(Fit.AllOut.Thefts.Mon)$coef[20,4]
Summary.Table.AllOut.Mon$AugPValue[3] <- summary(Fit.AllOut.Narcotics.Mon)$coef[20,4]
Summary.Table.AllOut.Mon$AugPValue[4] <- summary(Fit.AllOut.Battery.Mon)$coef[20,4]
Summary.Table.AllOut.Mon$AugPValue[5] <- summary(Fit.AllOut.CriminalDamage.Mon)$coef[20,4]
Summary.Table.AllOut.Mon$AugPValue[6] <- summary(Fit.AllOut.MotorVehicleTheft.Mon)$coef[20,4]
Summary.Table.AllOut.Mon$AugPValue[7] <- summary(Fit.AllOut.Robbery.Mon)$coef[20,4]
Summary.Table.AllOut.Mon$AugPValue[8] <- summary(Fit.AllOut.Assault.Mon)$coef[20,4]
Summary.Table.AllOut.Mon$AugPValue[9] <- summary(Fit.AllOut.Burglary.Mon)$coef[20,4]
Summary.Table.AllOut.Mon$AugPValue[10] <- summary(Fit.AllOut.Homicide.Mon)$coef[20,4]
Summary.Table.AllOut.Mon$AugPValue[11] <- summary(Fit.AllOut.DeceptivePractice.Mon)$coef[20,4]

Summary.Table.AllOut.Mon$SepPValue[1] <- summary(Fit.AllOut.AllCrimes.Mon)$coef[21,4]
Summary.Table.AllOut.Mon$SepPValue[2] <- summary(Fit.AllOut.Thefts.Mon)$coef[21,4]
Summary.Table.AllOut.Mon$SepPValue[3] <- summary(Fit.AllOut.Narcotics.Mon)$coef[21,4]
Summary.Table.AllOut.Mon$SepPValue[4] <- summary(Fit.AllOut.Battery.Mon)$coef[21,4]
Summary.Table.AllOut.Mon$SepPValue[5] <- summary(Fit.AllOut.CriminalDamage.Mon)$coef[21,4]
Summary.Table.AllOut.Mon$SepPValue[6] <- summary(Fit.AllOut.MotorVehicleTheft.Mon)$coef[21,4]
Summary.Table.AllOut.Mon$SepPValue[7] <- summary(Fit.AllOut.Robbery.Mon)$coef[21,4]
Summary.Table.AllOut.Mon$SepPValue[8] <- summary(Fit.AllOut.Assault.Mon)$coef[21,4]
Summary.Table.AllOut.Mon$SepPValue[9] <- summary(Fit.AllOut.Burglary.Mon)$coef[21,4]
Summary.Table.AllOut.Mon$SepPValue[10] <- summary(Fit.AllOut.Homicide.Mon)$coef[21,4]
Summary.Table.AllOut.Mon$SepPValue[11] <- summary(Fit.AllOut.DeceptivePractice.Mon)$coef[21,4]

Summary.Table.AllOut.Mon$OctPValue[1] <- summary(Fit.AllOut.AllCrimes.Mon)$coef[22,4]
Summary.Table.AllOut.Mon$OctPValue[2] <- summary(Fit.AllOut.Thefts.Mon)$coef[22,4]
Summary.Table.AllOut.Mon$OctPValue[3] <- summary(Fit.AllOut.Narcotics.Mon)$coef[22,4]
Summary.Table.AllOut.Mon$OctPValue[4] <- summary(Fit.AllOut.Battery.Mon)$coef[22,4]
Summary.Table.AllOut.Mon$OctPValue[5] <- summary(Fit.AllOut.CriminalDamage.Mon)$coef[22,4]
Summary.Table.AllOut.Mon$OctPValue[6] <- summary(Fit.AllOut.MotorVehicleTheft.Mon)$coef[22,4]
Summary.Table.AllOut.Mon$OctPValue[7] <- summary(Fit.AllOut.Robbery.Mon)$coef[22,4]
Summary.Table.AllOut.Mon$OctPValue[8] <- summary(Fit.AllOut.Assault.Mon)$coef[22,4]
Summary.Table.AllOut.Mon$OctPValue[9] <- summary(Fit.AllOut.Burglary.Mon)$coef[22,4]
Summary.Table.AllOut.Mon$OctPValue[10] <- summary(Fit.AllOut.Homicide.Mon)$coef[22,4]
Summary.Table.AllOut.Mon$OctPValue[11] <- summary(Fit.AllOut.DeceptivePractice.Mon)$coef[22,4]

Summary.Table.AllOut.Mon$NovPValue[1] <- summary(Fit.AllOut.AllCrimes.Mon)$coef[23,4]
Summary.Table.AllOut.Mon$NovPValue[2] <- summary(Fit.AllOut.Thefts.Mon)$coef[23,4]
Summary.Table.AllOut.Mon$NovPValue[3] <- summary(Fit.AllOut.Narcotics.Mon)$coef[23,4]
Summary.Table.AllOut.Mon$NovPValue[4] <- summary(Fit.AllOut.Battery.Mon)$coef[23,4]
Summary.Table.AllOut.Mon$NovPValue[5] <- summary(Fit.AllOut.CriminalDamage.Mon)$coef[23,4]
Summary.Table.AllOut.Mon$NovPValue[6] <- summary(Fit.AllOut.MotorVehicleTheft.Mon)$coef[23,4]
Summary.Table.AllOut.Mon$NovPValue[7] <- summary(Fit.AllOut.Robbery.Mon)$coef[23,4]
Summary.Table.AllOut.Mon$NovPValue[8] <- summary(Fit.AllOut.Assault.Mon)$coef[23,4]
Summary.Table.AllOut.Mon$NovPValue[9] <- summary(Fit.AllOut.Burglary.Mon)$coef[23,4]
Summary.Table.AllOut.Mon$NovPValue[10] <- summary(Fit.AllOut.Homicide.Mon)$coef[23,4]
Summary.Table.AllOut.Mon$NovPValue[11] <- summary(Fit.AllOut.DeceptivePractice.Mon)$coef[23,4]

Summary.Table.AllOut.Mon$DecPValue[1] <- summary(Fit.AllOut.AllCrimes.Mon)$coef[24,4]
Summary.Table.AllOut.Mon$DecPValue[2] <- summary(Fit.AllOut.Thefts.Mon)$coef[24,4]
Summary.Table.AllOut.Mon$DecPValue[3] <- summary(Fit.AllOut.Narcotics.Mon)$coef[24,4]
Summary.Table.AllOut.Mon$DecPValue[4] <- summary(Fit.AllOut.Battery.Mon)$coef[24,4]
Summary.Table.AllOut.Mon$DecPValue[5] <- summary(Fit.AllOut.CriminalDamage.Mon)$coef[24,4]
Summary.Table.AllOut.Mon$DecPValue[6] <- summary(Fit.AllOut.MotorVehicleTheft.Mon)$coef[24,4]
Summary.Table.AllOut.Mon$DecPValue[7] <- summary(Fit.AllOut.Robbery.Mon)$coef[24,4]
Summary.Table.AllOut.Mon$DecPValue[8] <- summary(Fit.AllOut.Assault.Mon)$coef[24,4]
Summary.Table.AllOut.Mon$DecPValue[9] <- summary(Fit.AllOut.Burglary.Mon)$coef[24,4]
Summary.Table.AllOut.Mon$DecPValue[10] <- summary(Fit.AllOut.Homicide.Mon)$coef[24,4]
Summary.Table.AllOut.Mon$DecPValue[11] <- summary(Fit.AllOut.DeceptivePractice.Mon)$coef[24,4]

Summary.Table.AllOut.Mon$JanStar[which(Summary.Table.AllOut.Mon$JanPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.AllOut.Mon$JanPValue<0.01)))
Summary.Table.AllOut.Mon$JanStar[which(Summary.Table.AllOut.Mon$JanPValue<0.05 & Summary.Table.AllOut.Mon$JanPValue>=0.01)] <- rep("*" , length(which(Summary.Table.AllOut.Mon$JanPValue<0.05 & Summary.Table.AllOut.Mon$JanPValue>=0.01)))
Summary.Table.AllOut.Mon$FebStar[which(Summary.Table.AllOut.Mon$FebPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.AllOut.Mon$FebPValue<0.01)))
Summary.Table.AllOut.Mon$FebStar[which(Summary.Table.AllOut.Mon$FebPValue<0.05 & Summary.Table.AllOut.Mon$FebPValue>=0.01)] <- rep("*" , length(which(Summary.Table.AllOut.Mon$FebPValue<0.05 & Summary.Table.AllOut.Mon$FebPValue>=0.01)))
Summary.Table.AllOut.Mon$MarStar[which(Summary.Table.AllOut.Mon$MarPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.AllOut.Mon$MarPValue<0.01)))
Summary.Table.AllOut.Mon$MarStar[which(Summary.Table.AllOut.Mon$MarPValue<0.05 & Summary.Table.AllOut.Mon$MarPValue>=0.01)] <- rep("*" , length(which(Summary.Table.AllOut.Mon$MarPValue<0.05 & Summary.Table.AllOut.Mon$MarPValue>=0.01)))
Summary.Table.AllOut.Mon$AprStar[which(Summary.Table.AllOut.Mon$AprPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.AllOut.Mon$AprPValue<0.01)))
Summary.Table.AllOut.Mon$AprStar[which(Summary.Table.AllOut.Mon$AprPValue<0.05 & Summary.Table.AllOut.Mon$AprPValue>=0.01)] <- rep("*" , length(which(Summary.Table.AllOut.Mon$AprPValue<0.05 & Summary.Table.AllOut.Mon$AprPValue>=0.01)))
Summary.Table.AllOut.Mon$MayStar[which(Summary.Table.AllOut.Mon$MayPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.AllOut.Mon$MayPValue<0.01)))
Summary.Table.AllOut.Mon$MayStar[which(Summary.Table.AllOut.Mon$MayPValue<0.05 & Summary.Table.AllOut.Mon$MayPValue>=0.01)] <- rep("*" , length(which(Summary.Table.AllOut.Mon$MayPValue<0.05 & Summary.Table.AllOut.Mon$MayPValue>=0.01)))
Summary.Table.AllOut.Mon$JunStar[which(Summary.Table.AllOut.Mon$JunPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.AllOut.Mon$JunPValue<0.01)))
Summary.Table.AllOut.Mon$JunStar[which(Summary.Table.AllOut.Mon$JunPValue<0.05 & Summary.Table.AllOut.Mon$JunPValue>=0.01)] <- rep("*" , length(which(Summary.Table.AllOut.Mon$JunPValue<0.05 & Summary.Table.AllOut.Mon$JunPValue>=0.01)))
Summary.Table.AllOut.Mon$JulStar[which(Summary.Table.AllOut.Mon$JulPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.AllOut.Mon$JulPValue<0.01)))
Summary.Table.AllOut.Mon$JulStar[which(Summary.Table.AllOut.Mon$JulPValue<0.05 & Summary.Table.AllOut.Mon$JulPValue>=0.01)] <- rep("*" , length(which(Summary.Table.AllOut.Mon$JulPValue<0.05 & Summary.Table.AllOut.Mon$JulPValue>=0.01)))
Summary.Table.AllOut.Mon$AugStar[which(Summary.Table.AllOut.Mon$AugPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.AllOut.Mon$AugPValue<0.01)))
Summary.Table.AllOut.Mon$AugStar[which(Summary.Table.AllOut.Mon$AugPValue<0.05 & Summary.Table.AllOut.Mon$AugPValue>=0.01)] <- rep("*" , length(which(Summary.Table.AllOut.Mon$AugPValue<0.05 & Summary.Table.AllOut.Mon$AugPValue>=0.01)))
Summary.Table.AllOut.Mon$SepStar[which(Summary.Table.AllOut.Mon$SepPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.AllOut.Mon$SepPValue<0.01)))
Summary.Table.AllOut.Mon$SepStar[which(Summary.Table.AllOut.Mon$SepPValue<0.05 & Summary.Table.AllOut.Mon$SepPValue>=0.01)] <- rep("*" , length(which(Summary.Table.AllOut.Mon$SepPValue<0.05 & Summary.Table.AllOut.Mon$SepPValue>=0.01)))
Summary.Table.AllOut.Mon$OctStar[which(Summary.Table.AllOut.Mon$OctPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.AllOut.Mon$OctPValue<0.01)))
Summary.Table.AllOut.Mon$OctStar[which(Summary.Table.AllOut.Mon$OctPValue<0.05 & Summary.Table.AllOut.Mon$OctPValue>=0.01)] <- rep("*" , length(which(Summary.Table.AllOut.Mon$OctPValue<0.05 & Summary.Table.AllOut.Mon$OctPValue>=0.01)))
Summary.Table.AllOut.Mon$NovStar[which(Summary.Table.AllOut.Mon$NovPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.AllOut.Mon$NovPValue<0.01)))
Summary.Table.AllOut.Mon$NovStar[which(Summary.Table.AllOut.Mon$NovPValue<0.05 & Summary.Table.AllOut.Mon$NovPValue>=0.01)] <- rep("*" , length(which(Summary.Table.AllOut.Mon$NovPValue<0.05 & Summary.Table.AllOut.Mon$NovPValue>=0.01)))
Summary.Table.AllOut.Mon$DecStar[which(Summary.Table.AllOut.Mon$DecPValue<0.01)]                                           <- rep("**", length(which(Summary.Table.AllOut.Mon$DecPValue<0.01)))
Summary.Table.AllOut.Mon$DecStar[which(Summary.Table.AllOut.Mon$DecPValue<0.05 & Summary.Table.AllOut.Mon$DecPValue>=0.01)] <- rep("*" , length(which(Summary.Table.AllOut.Mon$DecPValue<0.05 & Summary.Table.AllOut.Mon$DecPValue>=0.01)))


                                            
 
                                                                                                                                             
Summary.Table.Alley.Mon[,-c(3,6,9,12,15,18,21,24,27,30,33,36)]   <- round(Summary.Table.Alley.Mon[,-c(3,6,9,12,15,18,21,24,27,30,33,36)] , digits=3)                                                                                                                                              
Summary.Table.OneOut.Mon[,-c(3,6,9,12,15,18,21,24,27,30,33,36)]  <- round(Summary.Table.OneOut.Mon[,-c(3,6,9,12,15,18,21,24,27,30,33,36)] , digits=3)                                                                                                                                              
Summary.Table.AllOut.Mon[,-c(3,6,9,12,15,18,21,24,27,30,33,36)]  <- round(Summary.Table.AllOut.Mon[,-c(3,6,9,12,15,18,21,24,27,30,33,36)] , digits=3)                                                                                                                                              

## ESTIMATES RATES AND STANDARD ERRORS BY COMMUNITY AREA

#for (i in 1:77) {
#  paste("Fit.AllOut.AllCrimes.CA", i, sep = "") <- glm(AllCrimes ~ offset(log(Duration)) + OutageInd, family=poisson(link=log), data=subset(Street.Lights.AllOut.Pois, community_area==i)) 

#}








