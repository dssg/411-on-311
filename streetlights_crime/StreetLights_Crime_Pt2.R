rm(list = ls())

setwd("C:/Users/Zach/Documents/UrbanCCD/Streetlights")

#Open Alley Lights and Crime
Alley.Lights<- read.csv(file="Alley_Lights_and_Crime.csv", head=TRUE)

#Open Street Lights One Out and Crime
Street.Lights.OneOut<- read.csv(file="Street_Lights_One_Out_and_Crime.csv", head=TRUE)

#Open Street Lights All Out and Crime
Street.Lights.AllOut<- read.csv(file="Street_Lights_All_Out_and_Crime.csv", head=TRUE)

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
Alley.Lights$Rate.AllCrimes.During         <- 30*Alley.Lights$Thefts.During           /Alley.Lights$OutageDuration

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
Street.Lights.OneOut$Rate.AllCrimes.During         <- 30*Street.Lights.OneOut$Thefts.During           /Street.Lights.OneOut$OutageDuration

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
Street.Lights.AllOut$Rate.AllCrimes.During         <- 30*Street.Lights.AllOut$Thefts.During           /Street.Lights.AllOut$OutageDuration



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
Alley.Lights$Rate.AllCrimes.Before         <- Alley.Lights$Thefts.Before           

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
Street.Lights.OneOut$Rate.AllCrimes.Before         <- Street.Lights.OneOut$Thefts.Before           

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
Street.Lights.AllOut$Rate.AllCrimes.Before         <- Street.Lights.AllOut$Thefts.Before           





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
Alley.Lights$Rate.AllCrimes.After         <- 30*Alley.Lights$Thefts.After           /Alley.Lights$After.Period.Duration

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
Street.Lights.OneOut$Rate.AllCrimes.After         <- 30*Street.Lights.OneOut$Thefts.After           /Street.Lights.OneOut$After.Period.Duration

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
Street.Lights.AllOut$Rate.AllCrimes.After         <- 30*Street.Lights.AllOut$Thefts.After           /Street.Lights.AllOut$After.Period.Duration



