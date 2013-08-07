rm(list = ls())

#Load Necessary Packages
library(plyr)


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

#Create Crime Rate Variables - During
Alley.Lights$Rate.Thefts.During            <- 30*Alley.Lights$Thefts.During           /Alley.Lights$OutageDuration
Alley.Lights$Rate.Narcotics.During         <- 30*Alley.Lights$Narcotics.During        /Alley.Lights$OutageDuration
Alley.Lights$Rate.Battery.During           <- 30*Alley.Lights$Battery.During          /Alley.Lights$OutageDuration
Alley.Lights$Rate.CriminalDamage.During    <- 30*Alley.Lights$CriminalDamage.During   /Alley.Lights$OutageDuration
Alley.Lights$Rate.MotorVehicleTheft.During <- 30*Alley.Lights$MotorVehicleTheft.During/Alley.Lights$OutageDuration
Alley.Lights$Rate.Robbery.During           <- 30*Alley.Lights$Robbery.During          /Alley.Lights$OutageDuration
Alley.Lights$Rate.Assault.During           <- 30*Alley.Lights$Assault.During          /Alley.Lights$OutageDuration
Alley.Lights$Rate.Burglary.During          <- 30*Alley.Lights$Burglary.During         /Alley.Lights$OutageDuration
Alley.Lights$Rate.Homicide.During          <- 30*Alley.Lights$Homicide.During         /Alley.Lights$OutageDuration
Alley.Lights$Rate.AlleyCrimes.During       <- 30*Alley.Lights$Crimes.Alley.During     /Alley.Lights$OutageDuration
Alley.Lights$Rate.AllCrimes.During         <- 30*Alley.Lights$Crimes.All.During       /Alley.Lights$OutageDuration

Street.Lights.OneOut$Rate.Thefts.During            <- 30*Street.Lights.OneOut$Thefts.During           /Street.Lights.OneOut$OutageDuration
Street.Lights.OneOut$Rate.Narcotics.During         <- 30*Street.Lights.OneOut$Narcotics.During        /Street.Lights.OneOut$OutageDuration
Street.Lights.OneOut$Rate.Battery.During           <- 30*Street.Lights.OneOut$Battery.During          /Street.Lights.OneOut$OutageDuration
Street.Lights.OneOut$Rate.CriminalDamage.During    <- 30*Street.Lights.OneOut$CriminalDamage.During   /Street.Lights.OneOut$OutageDuration
Street.Lights.OneOut$Rate.MotorVehicleTheft.During <- 30*Street.Lights.OneOut$MotorVehicleTheft.During/Street.Lights.OneOut$OutageDuration
Street.Lights.OneOut$Rate.Robbery.During           <- 30*Street.Lights.OneOut$Robbery.During          /Street.Lights.OneOut$OutageDuration
Street.Lights.OneOut$Rate.Assault.During           <- 30*Street.Lights.OneOut$Assault.During          /Street.Lights.OneOut$OutageDuration
Street.Lights.OneOut$Rate.Burglary.During          <- 30*Street.Lights.OneOut$Burglary.During         /Street.Lights.OneOut$OutageDuration
Street.Lights.OneOut$Rate.Homicide.During          <- 30*Street.Lights.OneOut$Homicide.During         /Street.Lights.OneOut$OutageDuration
Street.Lights.OneOut$Rate.SidewalkCrimes.During    <- 30*Street.Lights.OneOut$Crimes.Sidewalk.During  /Street.Lights.OneOut$OutageDuration
Street.Lights.OneOut$Rate.StreetCrimes.During      <- 30*Street.Lights.OneOut$Crimes.Street.During    /Street.Lights.OneOut$OutageDuration
Street.Lights.OneOut$Rate.AllCrimes.During         <- 30*Street.Lights.OneOut$Crimes.All.During       /Street.Lights.OneOut$OutageDuration

Street.Lights.AllOut$Rate.Thefts.During            <- 30*Street.Lights.AllOut$Thefts.During           /Street.Lights.AllOut$OutageDuration
Street.Lights.AllOut$Rate.Narcotics.During         <- 30*Street.Lights.AllOut$Narcotics.During        /Street.Lights.AllOut$OutageDuration
Street.Lights.AllOut$Rate.Battery.During           <- 30*Street.Lights.AllOut$Battery.During          /Street.Lights.AllOut$OutageDuration
Street.Lights.AllOut$Rate.CriminalDamage.During    <- 30*Street.Lights.AllOut$CriminalDamage.During   /Street.Lights.AllOut$OutageDuration
Street.Lights.AllOut$Rate.MotorVehicleTheft.During <- 30*Street.Lights.AllOut$MotorVehicleTheft.During/Street.Lights.AllOut$OutageDuration
Street.Lights.AllOut$Rate.Robbery.During           <- 30*Street.Lights.AllOut$Robbery.During          /Street.Lights.AllOut$OutageDuration
Street.Lights.AllOut$Rate.Assault.During           <- 30*Street.Lights.AllOut$Assault.During          /Street.Lights.AllOut$OutageDuration
Street.Lights.AllOut$Rate.Burglary.During          <- 30*Street.Lights.AllOut$Burglary.During         /Street.Lights.AllOut$OutageDuration
Street.Lights.AllOut$Rate.Homicide.During          <- 30*Street.Lights.AllOut$Homicide.During         /Street.Lights.AllOut$OutageDuration
Street.Lights.AllOut$Rate.SidewalkCrimes.During    <- 30*Street.Lights.AllOut$Crimes.Sidewalk.During  /Street.Lights.AllOut$OutageDuration
Street.Lights.AllOut$Rate.StreetCrimes.During      <- 30*Street.Lights.AllOut$Crimes.Street.During    /Street.Lights.AllOut$OutageDuration
Street.Lights.AllOut$Rate.AllCrimes.During         <- 30*Street.Lights.AllOut$Crimes.All.During       /Street.Lights.AllOut$OutageDuration



#Create Crime Rate Variables - Before
Alley.Lights$Rate.Thefts.Before            <- Alley.Lights$Thefts.Before           
Alley.Lights$Rate.Narcotics.Before         <- Alley.Lights$Narcotics.Before        
Alley.Lights$Rate.Battery.Before           <- Alley.Lights$Battery.Before          
Alley.Lights$Rate.CriminalDamage.Before    <- Alley.Lights$CriminalDamage.Before   
Alley.Lights$Rate.MotorVehicleTheft.Before <- Alley.Lights$MotorVehicleTheft.Before
Alley.Lights$Rate.Robbery.Before           <- Alley.Lights$Robbery.Before          
Alley.Lights$Rate.Assault.Before           <- Alley.Lights$Assault.Before          
Alley.Lights$Rate.Burglary.Before          <- Alley.Lights$Burglary.Before         
Alley.Lights$Rate.Homicide.Before          <- Alley.Lights$Homicide.Before         
Alley.Lights$Rate.AlleyCrimes.Before       <- Alley.Lights$Crimes.Alley.Before     
Alley.Lights$Rate.AllCrimes.Before         <- Alley.Lights$Crimes.All.Before           

Street.Lights.OneOut$Rate.Thefts.Before            <- Street.Lights.OneOut$Thefts.Before           
Street.Lights.OneOut$Rate.Narcotics.Before         <- Street.Lights.OneOut$Narcotics.Before        
Street.Lights.OneOut$Rate.Battery.Before           <- Street.Lights.OneOut$Battery.Before          
Street.Lights.OneOut$Rate.CriminalDamage.Before    <- Street.Lights.OneOut$CriminalDamage.Before   
Street.Lights.OneOut$Rate.MotorVehicleTheft.Before <- Street.Lights.OneOut$MotorVehicleTheft.Before
Street.Lights.OneOut$Rate.Robbery.Before           <- Street.Lights.OneOut$Robbery.Before          
Street.Lights.OneOut$Rate.Assault.Before           <- Street.Lights.OneOut$Assault.Before          
Street.Lights.OneOut$Rate.Burglary.Before          <- Street.Lights.OneOut$Burglary.Before         
Street.Lights.OneOut$Rate.Homicide.Before          <- Street.Lights.OneOut$Homicide.Before         
Street.Lights.OneOut$Rate.SidewalkCrimes.Before    <- Street.Lights.OneOut$Crimes.Sidewalk.Before  
Street.Lights.OneOut$Rate.StreetCrimes.Before      <- Street.Lights.OneOut$Crimes.Street.Before    
Street.Lights.OneOut$Rate.AllCrimes.Before         <- Street.Lights.OneOut$Crimes.All.Before           

Street.Lights.AllOut$Rate.Thefts.Before            <- Street.Lights.AllOut$Thefts.Before           
Street.Lights.AllOut$Rate.Narcotics.Before         <- Street.Lights.AllOut$Narcotics.Before        
Street.Lights.AllOut$Rate.Battery.Before           <- Street.Lights.AllOut$Battery.Before          
Street.Lights.AllOut$Rate.CriminalDamage.Before    <- Street.Lights.AllOut$CriminalDamage.Before   
Street.Lights.AllOut$Rate.MotorVehicleTheft.Before <- Street.Lights.AllOut$MotorVehicleTheft.Before
Street.Lights.AllOut$Rate.Robbery.Before           <- Street.Lights.AllOut$Robbery.Before          
Street.Lights.AllOut$Rate.Assault.Before           <- Street.Lights.AllOut$Assault.Before          
Street.Lights.AllOut$Rate.Burglary.Before          <- Street.Lights.AllOut$Burglary.Before         
Street.Lights.AllOut$Rate.Homicide.Before          <- Street.Lights.AllOut$Homicide.Before         
Street.Lights.AllOut$Rate.SidewalkCrimes.Before    <- Street.Lights.AllOut$Crimes.Sidewalk.Before  
Street.Lights.AllOut$Rate.StreetCrimes.Before      <- Street.Lights.AllOut$Crimes.Street.Before    
Street.Lights.AllOut$Rate.AllCrimes.Before         <- Street.Lights.AllOut$Crimes.All.Before           





#Create Crime Rate Variables - After
Alley.Lights$Rate.Thefts.After            <- 30*Alley.Lights$Thefts.After           /Alley.Lights$After.Period.Duration
Alley.Lights$Rate.Narcotics.After         <- 30*Alley.Lights$Narcotics.After        /Alley.Lights$After.Period.Duration
Alley.Lights$Rate.Battery.After           <- 30*Alley.Lights$Battery.After          /Alley.Lights$After.Period.Duration
Alley.Lights$Rate.CriminalDamage.After    <- 30*Alley.Lights$CriminalDamage.After   /Alley.Lights$After.Period.Duration
Alley.Lights$Rate.MotorVehicleTheft.After <- 30*Alley.Lights$MotorVehicleTheft.After/Alley.Lights$After.Period.Duration
Alley.Lights$Rate.Robbery.After           <- 30*Alley.Lights$Robbery.After          /Alley.Lights$After.Period.Duration
Alley.Lights$Rate.Assault.After           <- 30*Alley.Lights$Assault.After          /Alley.Lights$After.Period.Duration
Alley.Lights$Rate.Burglary.After          <- 30*Alley.Lights$Burglary.After         /Alley.Lights$After.Period.Duration
Alley.Lights$Rate.Homicide.After          <- 30*Alley.Lights$Homicide.After         /Alley.Lights$After.Period.Duration
Alley.Lights$Rate.AlleyCrimes.After       <- 30*Alley.Lights$Crimes.Alley.After     /Alley.Lights$After.Period.Duration
Alley.Lights$Rate.AllCrimes.After         <- 30*Alley.Lights$Crimes.All.After       /Alley.Lights$After.Period.Duration

Street.Lights.OneOut$Rate.Thefts.After            <- 30*Street.Lights.OneOut$Thefts.After           /Street.Lights.OneOut$After.Period.Duration
Street.Lights.OneOut$Rate.Narcotics.After         <- 30*Street.Lights.OneOut$Narcotics.After        /Street.Lights.OneOut$After.Period.Duration
Street.Lights.OneOut$Rate.Battery.After           <- 30*Street.Lights.OneOut$Battery.After          /Street.Lights.OneOut$After.Period.Duration
Street.Lights.OneOut$Rate.CriminalDamage.After    <- 30*Street.Lights.OneOut$CriminalDamage.After   /Street.Lights.OneOut$After.Period.Duration
Street.Lights.OneOut$Rate.MotorVehicleTheft.After <- 30*Street.Lights.OneOut$MotorVehicleTheft.After/Street.Lights.OneOut$After.Period.Duration
Street.Lights.OneOut$Rate.Robbery.After           <- 30*Street.Lights.OneOut$Robbery.After          /Street.Lights.OneOut$After.Period.Duration
Street.Lights.OneOut$Rate.Assault.After           <- 30*Street.Lights.OneOut$Assault.After          /Street.Lights.OneOut$After.Period.Duration
Street.Lights.OneOut$Rate.Burglary.After          <- 30*Street.Lights.OneOut$Burglary.After         /Street.Lights.OneOut$After.Period.Duration
Street.Lights.OneOut$Rate.Homicide.After          <- 30*Street.Lights.OneOut$Homicide.After         /Street.Lights.OneOut$After.Period.Duration
Street.Lights.OneOut$Rate.SidewalkCrimes.After    <- 30*Street.Lights.OneOut$Crimes.Sidewalk.After  /Street.Lights.OneOut$After.Period.Duration
Street.Lights.OneOut$Rate.StreetCrimes.After      <- 30*Street.Lights.OneOut$Crimes.Street.After    /Street.Lights.OneOut$After.Period.Duration
Street.Lights.OneOut$Rate.AllCrimes.After         <- 30*Street.Lights.OneOut$Crimes.All.After       /Street.Lights.OneOut$After.Period.Duration

Street.Lights.AllOut$Rate.Thefts.After            <- 30*Street.Lights.AllOut$Thefts.After           /Street.Lights.AllOut$After.Period.Duration
Street.Lights.AllOut$Rate.Narcotics.After         <- 30*Street.Lights.AllOut$Narcotics.After        /Street.Lights.AllOut$After.Period.Duration
Street.Lights.AllOut$Rate.Battery.After           <- 30*Street.Lights.AllOut$Battery.After          /Street.Lights.AllOut$After.Period.Duration
Street.Lights.AllOut$Rate.CriminalDamage.After    <- 30*Street.Lights.AllOut$CriminalDamage.After   /Street.Lights.AllOut$After.Period.Duration
Street.Lights.AllOut$Rate.MotorVehicleTheft.After <- 30*Street.Lights.AllOut$MotorVehicleTheft.After/Street.Lights.AllOut$After.Period.Duration
Street.Lights.AllOut$Rate.Robbery.After           <- 30*Street.Lights.AllOut$Robbery.After          /Street.Lights.AllOut$After.Period.Duration
Street.Lights.AllOut$Rate.Assault.After           <- 30*Street.Lights.AllOut$Assault.After          /Street.Lights.AllOut$After.Period.Duration
Street.Lights.AllOut$Rate.Burglary.After          <- 30*Street.Lights.AllOut$Burglary.After         /Street.Lights.AllOut$After.Period.Duration
Street.Lights.AllOut$Rate.Homicide.After          <- 30*Street.Lights.AllOut$Homicide.After         /Street.Lights.AllOut$After.Period.Duration
Street.Lights.AllOut$Rate.SidewalkCrimes.After    <- 30*Street.Lights.AllOut$Crimes.Sidewalk.After  /Street.Lights.AllOut$After.Period.Duration
Street.Lights.AllOut$Rate.StreetCrimes.After      <- 30*Street.Lights.AllOut$Crimes.Street.After    /Street.Lights.AllOut$After.Period.Duration
Street.Lights.AllOut$Rate.AllCrimes.After         <- 30*Street.Lights.AllOut$Crimes.All.After       /Street.Lights.AllOut$After.Period.Duration


#Create Summary Tables
Summary.Table.Alley  <- data.frame(matrix(ncol = 4, nrow = 11), row.names = c("All Crimes", "Alley Crimes", 
                                                                              "Thefts", "Narcotics", "Battery", "Criminal Damage", 
                                                                              "Motor Vehicle Theft", "Robbery", "Assault", "Burglary", "Homicide"))
Summary.Table.OneOut <- data.frame(matrix(ncol = 4, nrow = 12), row.names = c("All Crimes", "Sidewalk Crimes", "Street Crimes",
                                                                              "Thefts", "Narcotics", "Battery", "Criminal Damage", 
                                                                              "Motor Vehicle Theft", "Robbery", "Assault", "Burglary", "Homicide"))
Summary.Table.AllOut <- data.frame(matrix(ncol = 4, nrow = 12), row.names = c("All Crimes", "Sidewalk Crimes", "Street Crimes",
                                                                              "Thefts", "Narcotics", "Battery", "Criminal Damage", 
                                                                              "Motor Vehicle Theft", "Robbery", "Assault", "Burglary", "Homicide"))
Summary.Table.Alley  <- rename(Summary.Table.Alley,  c("X1"="Before", "X2"="During", "X3"="After", "X4"="AvgBeforeAfter"))
Summary.Table.OneOut <- rename(Summary.Table.OneOut, c("X1"="Before", "X2"="During", "X3"="After", "X4"="AvgBeforeAfter"))
Summary.Table.AllOut <- rename(Summary.Table.AllOut, c("X1"="Before", "X2"="During", "X3"="After", "X4"="AvgBeforeAfter"))



Summary.Table.Alley[1,1]  <- 30*sum(Alley.Lights$Crimes.All.Before)       /(30*nrow(Alley.Lights))
Summary.Table.Alley[2,1]  <- 30*sum(Alley.Lights$Crimes.Alley.Before)     /(30*nrow(Alley.Lights))
Summary.Table.Alley[3,1]  <- 30*sum(Alley.Lights$Thefts.Before)           /(30*nrow(Alley.Lights))
Summary.Table.Alley[4,1]  <- 30*sum(Alley.Lights$Narcotics.Before)        /(30*nrow(Alley.Lights))
Summary.Table.Alley[5,1]  <- 30*sum(Alley.Lights$Battery.Before)          /(30*nrow(Alley.Lights))
Summary.Table.Alley[6,1]  <- 30*sum(Alley.Lights$CriminalDamage.Before)   /(30*nrow(Alley.Lights))
Summary.Table.Alley[7,1]  <- 30*sum(Alley.Lights$MotorVehicleTheft.Before)/(30*nrow(Alley.Lights))
Summary.Table.Alley[8,1]  <- 30*sum(Alley.Lights$Robbery.Before)          /(30*nrow(Alley.Lights))
Summary.Table.Alley[9,1]  <- 30*sum(Alley.Lights$Assault.Before)          /(30*nrow(Alley.Lights))
Summary.Table.Alley[10,1] <- 30*sum(Alley.Lights$Burglary.Before)         /(30*nrow(Alley.Lights))
Summary.Table.Alley[11,1] <- 30*sum(Alley.Lights$Homicide.Before)         /(30*nrow(Alley.Lights))
Summary.Table.Alley[1,2]  <- 30*sum(Alley.Lights$Crimes.All.During)       /sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[2,2]  <- 30*sum(Alley.Lights$Crimes.Alley.During)     /sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[3,2]  <- 30*sum(Alley.Lights$Thefts.During)           /sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[4,2]  <- 30*sum(Alley.Lights$Narcotics.During)        /sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[5,2]  <- 30*sum(Alley.Lights$Battery.During)          /sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[6,2]  <- 30*sum(Alley.Lights$CriminalDamage.During)   /sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[7,2]  <- 30*sum(Alley.Lights$MotorVehicleTheft.During)/sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[8,2]  <- 30*sum(Alley.Lights$Robbery.During)          /sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[9,2]  <- 30*sum(Alley.Lights$Assault.During)          /sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[10,2] <- 30*sum(Alley.Lights$Burglary.During)         /sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[11,2] <- 30*sum(Alley.Lights$Homicide.During)         /sum(Alley.Lights$OutageDuration)
Summary.Table.Alley[1,3]  <- 30*sum(Alley.Lights$Crimes.All.After)        /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[2,3]  <- 30*sum(Alley.Lights$Crimes.Alley.After)      /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[3,3]  <- 30*sum(Alley.Lights$Thefts.After)            /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[4,3]  <- 30*sum(Alley.Lights$Narcotics.After)         /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[5,3]  <- 30*sum(Alley.Lights$Battery.After)           /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[6,3]  <- 30*sum(Alley.Lights$CriminalDamage.After)    /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[7,3]  <- 30*sum(Alley.Lights$MotorVehicleTheft.After) /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[8,3]  <- 30*sum(Alley.Lights$Robbery.After)           /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[9,3]  <- 30*sum(Alley.Lights$Assault.After)           /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[10,3] <- 30*sum(Alley.Lights$Burglary.After)          /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[11,3] <- 30*sum(Alley.Lights$Homicide.After)          /sum(Alley.Lights$After.Period.Duration)
Summary.Table.Alley[1,4]  <- 30*(sum(Alley.Lights$Crimes.All.Before)       +sum(Alley.Lights$Crimes.All.After))       /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[2,4]  <- 30*(sum(Alley.Lights$Crimes.Alley.Before)     +sum(Alley.Lights$Crimes.Alley.After))     /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[3,4]  <- 30*(sum(Alley.Lights$Thefts.Before)           +sum(Alley.Lights$Thefts.After))           /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[4,4]  <- 30*(sum(Alley.Lights$Narcotics.Before)        +sum(Alley.Lights$Narcotics.After))        /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[5,4]  <- 30*(sum(Alley.Lights$Battery.Before)          +sum(Alley.Lights$Battery.After))          /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[6,4]  <- 30*(sum(Alley.Lights$CriminalDamage.Before)   +sum(Alley.Lights$CriminalDamage.After))   /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[7,4]  <- 30*(sum(Alley.Lights$MotorVehicleTheft.Before)+sum(Alley.Lights$MotorVehicleTheft.After))/(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[8,4]  <- 30*(sum(Alley.Lights$Robbery.Before)          +sum(Alley.Lights$Robbery.After))          /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[9,4]  <- 30*(sum(Alley.Lights$Assault.Before)          +sum(Alley.Lights$Assault.After))          /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[10,4] <- 30*(sum(Alley.Lights$Burglary.Before)         +sum(Alley.Lights$Burglary.After))         /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))
Summary.Table.Alley[11,4] <- 30*(sum(Alley.Lights$Homicide.Before)         +sum(Alley.Lights$Homicide.After))         /(30*nrow(Alley.Lights)+sum(Alley.Lights$After.Period.Duration))

Summary.Table.OneOut[1,1]  <- 30*sum(Street.Lights.OneOut$Crimes.All.Before)       /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[2,1]  <- 30*sum(Street.Lights.OneOut$Crimes.Sidewalk.Before)  /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[3,1]  <- 30*sum(Street.Lights.OneOut$Crimes.Street.Before)    /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[4,1]  <- 30*sum(Street.Lights.OneOut$Thefts.Before)           /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[5,1]  <- 30*sum(Street.Lights.OneOut$Narcotics.Before)        /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[6,1]  <- 30*sum(Street.Lights.OneOut$Battery.Before)          /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[7,1]  <- 30*sum(Street.Lights.OneOut$CriminalDamage.Before)   /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[8,1]  <- 30*sum(Street.Lights.OneOut$MotorVehicleTheft.Before)/(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[9,1]  <- 30*sum(Street.Lights.OneOut$Robbery.Before)          /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[10,1] <- 30*sum(Street.Lights.OneOut$Assault.Before)          /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[11,1] <- 30*sum(Street.Lights.OneOut$Burglary.Before)         /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[12,1] <- 30*sum(Street.Lights.OneOut$Homicide.Before)         /(30*nrow(Street.Lights.OneOut))
Summary.Table.OneOut[1,2]  <- 30*sum(Street.Lights.OneOut$Crimes.All.During)       /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[2,2]  <- 30*sum(Street.Lights.OneOut$Crimes.Sidewalk.During)  /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[3,2]  <- 30*sum(Street.Lights.OneOut$Crimes.Street.During)    /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[4,2]  <- 30*sum(Street.Lights.OneOut$Thefts.During)           /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[5,2]  <- 30*sum(Street.Lights.OneOut$Narcotics.During)        /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[6,2]  <- 30*sum(Street.Lights.OneOut$Battery.During)          /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[7,2]  <- 30*sum(Street.Lights.OneOut$CriminalDamage.During)   /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[8,2]  <- 30*sum(Street.Lights.OneOut$MotorVehicleTheft.During)/sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[9,2]  <- 30*sum(Street.Lights.OneOut$Robbery.During)          /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[10,2] <- 30*sum(Street.Lights.OneOut$Assault.During)          /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[11,2] <- 30*sum(Street.Lights.OneOut$Burglary.During)         /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[12,2] <- 30*sum(Street.Lights.OneOut$Homicide.During)         /sum(Street.Lights.OneOut$OutageDuration)
Summary.Table.OneOut[1,3]  <- 30*sum(Street.Lights.OneOut$Crimes.All.After)        /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[2,3]  <- 30*sum(Street.Lights.OneOut$Crimes.Sidewalk.After)   /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[3,3]  <- 30*sum(Street.Lights.OneOut$Crimes.Street.After)     /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[4,3]  <- 30*sum(Street.Lights.OneOut$Thefts.After)            /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[5,3]  <- 30*sum(Street.Lights.OneOut$Narcotics.After)         /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[6,3]  <- 30*sum(Street.Lights.OneOut$Battery.After)           /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[7,3]  <- 30*sum(Street.Lights.OneOut$CriminalDamage.After)    /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[8,3]  <- 30*sum(Street.Lights.OneOut$MotorVehicleTheft.After) /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[9,3]  <- 30*sum(Street.Lights.OneOut$Robbery.After)           /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[10,3] <- 30*sum(Street.Lights.OneOut$Assault.After)           /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[11,3] <- 30*sum(Street.Lights.OneOut$Burglary.After)          /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[12,3] <- 30*sum(Street.Lights.OneOut$Homicide.After)          /sum(Street.Lights.OneOut$After.Period.Duration)
Summary.Table.OneOut[1,4]  <- 30*(sum(Street.Lights.OneOut$Crimes.All.Before)       +sum(Street.Lights.OneOut$Crimes.All.After))       /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[2,4]  <- 30*(sum(Street.Lights.OneOut$Crimes.Sidewalk.Before)  +sum(Street.Lights.OneOut$Crimes.Sidewalk.After))  /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[3,4]  <- 30*(sum(Street.Lights.OneOut$Crimes.Street.Before)    +sum(Street.Lights.OneOut$Crimes.Street.After))    /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[4,4]  <- 30*(sum(Street.Lights.OneOut$Thefts.Before)           +sum(Street.Lights.OneOut$Thefts.After))           /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[5,4]  <- 30*(sum(Street.Lights.OneOut$Narcotics.Before)        +sum(Street.Lights.OneOut$Narcotics.After))        /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[6,4]  <- 30*(sum(Street.Lights.OneOut$Battery.Before)          +sum(Street.Lights.OneOut$Battery.After))          /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[7,4]  <- 30*(sum(Street.Lights.OneOut$CriminalDamage.Before)   +sum(Street.Lights.OneOut$CriminalDamage.After))   /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[8,4]  <- 30*(sum(Street.Lights.OneOut$MotorVehicleTheft.Before)+sum(Street.Lights.OneOut$MotorVehicleTheft.After))/(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[9,4]  <- 30*(sum(Street.Lights.OneOut$Robbery.Before)          +sum(Street.Lights.OneOut$Robbery.After))          /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[10,4] <- 30*(sum(Street.Lights.OneOut$Assault.Before)          +sum(Street.Lights.OneOut$Assault.After))          /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[11,4] <- 30*(sum(Street.Lights.OneOut$Burglary.Before)         +sum(Street.Lights.OneOut$Burglary.After))         /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))
Summary.Table.OneOut[12,4] <- 30*(sum(Street.Lights.OneOut$Homicide.Before)         +sum(Street.Lights.OneOut$Homicide.After))         /(30*nrow(Street.Lights.OneOut)+sum(Street.Lights.OneOut$After.Period.Duration))


Summary.Table.AllOut[1,1]  <- 30*sum(Street.Lights.AllOut$Crimes.All.Before)       /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[2,1]  <- 30*sum(Street.Lights.AllOut$Crimes.Sidewalk.Before)  /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[3,1]  <- 30*sum(Street.Lights.AllOut$Crimes.Street.Before)    /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[4,1]  <- 30*sum(Street.Lights.AllOut$Thefts.Before)           /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[5,1]  <- 30*sum(Street.Lights.AllOut$Narcotics.Before)        /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[6,1]  <- 30*sum(Street.Lights.AllOut$Battery.Before)          /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[7,1]  <- 30*sum(Street.Lights.AllOut$CriminalDamage.Before)   /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[8,1]  <- 30*sum(Street.Lights.AllOut$MotorVehicleTheft.Before)/(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[9,1]  <- 30*sum(Street.Lights.AllOut$Robbery.Before)          /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[10,1] <- 30*sum(Street.Lights.AllOut$Assault.Before)          /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[11,1] <- 30*sum(Street.Lights.AllOut$Burglary.Before)         /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[12,1] <- 30*sum(Street.Lights.AllOut$Homicide.Before)         /(30*nrow(Street.Lights.AllOut))
Summary.Table.AllOut[1,2]  <- 30*sum(Street.Lights.AllOut$Crimes.All.During)       /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[2,2]  <- 30*sum(Street.Lights.AllOut$Crimes.Sidewalk.During)  /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[3,2]  <- 30*sum(Street.Lights.AllOut$Crimes.Street.During)    /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[4,2]  <- 30*sum(Street.Lights.AllOut$Thefts.During)           /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[5,2]  <- 30*sum(Street.Lights.AllOut$Narcotics.During)        /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[6,2]  <- 30*sum(Street.Lights.AllOut$Battery.During)          /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[7,2]  <- 30*sum(Street.Lights.AllOut$CriminalDamage.During)   /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[8,2]  <- 30*sum(Street.Lights.AllOut$MotorVehicleTheft.During)/sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[9,2]  <- 30*sum(Street.Lights.AllOut$Robbery.During)          /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[10,2] <- 30*sum(Street.Lights.AllOut$Assault.During)          /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[11,2] <- 30*sum(Street.Lights.AllOut$Burglary.During)         /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[12,2] <- 30*sum(Street.Lights.AllOut$Homicide.During)         /sum(Street.Lights.AllOut$OutageDuration)
Summary.Table.AllOut[1,3]  <- 30*sum(Street.Lights.AllOut$Crimes.All.After)        /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[2,3]  <- 30*sum(Street.Lights.AllOut$Crimes.Sidewalk.After)   /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[3,3]  <- 30*sum(Street.Lights.AllOut$Crimes.Street.After)     /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[4,3]  <- 30*sum(Street.Lights.AllOut$Thefts.After)            /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[5,3]  <- 30*sum(Street.Lights.AllOut$Narcotics.After)         /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[6,3]  <- 30*sum(Street.Lights.AllOut$Battery.After)           /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[7,3]  <- 30*sum(Street.Lights.AllOut$CriminalDamage.After)    /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[8,3]  <- 30*sum(Street.Lights.AllOut$MotorVehicleTheft.After) /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[9,3]  <- 30*sum(Street.Lights.AllOut$Robbery.After)           /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[10,3] <- 30*sum(Street.Lights.AllOut$Assault.After)           /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[11,3] <- 30*sum(Street.Lights.AllOut$Burglary.After)          /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[12,3] <- 30*sum(Street.Lights.AllOut$Homicide.After)          /sum(Street.Lights.AllOut$After.Period.Duration)
Summary.Table.AllOut[1,4]  <- 30*(sum(Street.Lights.AllOut$Crimes.All.Before)       +sum(Street.Lights.AllOut$Crimes.All.After))       /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[2,4]  <- 30*(sum(Street.Lights.AllOut$Crimes.Sidewalk.Before)  +sum(Street.Lights.AllOut$Crimes.Sidewalk.After))  /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[3,4]  <- 30*(sum(Street.Lights.AllOut$Crimes.Street.Before)    +sum(Street.Lights.AllOut$Crimes.Street.After))    /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[4,4]  <- 30*(sum(Street.Lights.AllOut$Thefts.Before)           +sum(Street.Lights.AllOut$Thefts.After))           /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[5,4]  <- 30*(sum(Street.Lights.AllOut$Narcotics.Before)        +sum(Street.Lights.AllOut$Narcotics.After))        /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[6,4]  <- 30*(sum(Street.Lights.AllOut$Battery.Before)          +sum(Street.Lights.AllOut$Battery.After))          /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[7,4]  <- 30*(sum(Street.Lights.AllOut$CriminalDamage.Before)   +sum(Street.Lights.AllOut$CriminalDamage.After))   /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[8,4]  <- 30*(sum(Street.Lights.AllOut$MotorVehicleTheft.Before)+sum(Street.Lights.AllOut$MotorVehicleTheft.After))/(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[9,4]  <- 30*(sum(Street.Lights.AllOut$Robbery.Before)          +sum(Street.Lights.AllOut$Robbery.After))          /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[10,4] <- 30*(sum(Street.Lights.AllOut$Assault.Before)          +sum(Street.Lights.AllOut$Assault.After))          /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[11,4] <- 30*(sum(Street.Lights.AllOut$Burglary.Before)         +sum(Street.Lights.AllOut$Burglary.After))         /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))
Summary.Table.AllOut[12,4] <- 30*(sum(Street.Lights.AllOut$Homicide.Before)         +sum(Street.Lights.AllOut$Homicide.After))         /(30*nrow(Street.Lights.AllOut)+sum(Street.Lights.AllOut$After.Period.Duration))


Summary.Table.Alley$AbsDiff  <- Summary.Table.Alley$During  - Summary.Table.Alley$AvgBeforeAfter
Summary.Table.OneOut$AbsDiff <- Summary.Table.OneOut$During - Summary.Table.OneOut$AvgBeforeAfter
Summary.Table.AllOut$AbsDiff <- Summary.Table.AllOut$During - Summary.Table.AllOut$AvgBeforeAfter

Summary.Table.Alley$PctDiff  <- 100*Summary.Table.Alley$AbsDiff /Summary.Table.Alley$AvgBeforeAfter
Summary.Table.OneOut$PctDiff <- 100*Summary.Table.OneOut$AbsDiff/Summary.Table.OneOut$AvgBeforeAfter
Summary.Table.AllOut$PctDiff <- 100*Summary.Table.AllOut$AbsDiff/Summary.Table.AllOut$AvgBeforeAfter

Summary.Table.Alley  <- round(Summary.Table.Alley , digits=3)
Summary.Table.OneOut <- round(Summary.Table.OneOut, digits=3)
Summary.Table.AllOut <- round(Summary.Table.AllOut, digits=3)


