

setwd("/mnt/data1/Indices/streetlights")

#Open Crime Portal Data
Crime <- read.csv(file="Crimes_-_Feb_15__2012_to_August_6__2013_-_Outdoor.csv", head=TRUE)
crime.type.tab <- as.data.frame(summary(Crime$Primary.Type))
crime.location.tab <- as.data.frame(summary(Crime$Location.Description))

#Keep only Crime Types of Interest
Crime <- Crime[Crime$Primary.Type %in% c("THEFT", "NARCOTICS", "BATTERY", "CRIMINAL DAMAGE", "MOTOR VEHICLE THEFT", "ROBBERY", "ASSAULT", "BURGLARY", "HOMICIDE", "DECEPTIVE PRACTICE"),]
crime.type.tab.2 <- as.data.frame(summary(Crime$Primary.Type))

#Create New Date Variable for Crime
Crime$DateCrime <- as.Date(substr(as.character(Crime$Date), 1, nchar(as.character(Crime$Date))-5), "%m/%d/%Y")

#Check # of NA's in Crime Location Data
summary(Crime$X.Coordinate)
#4446 out of 210898 have no X and Y coordinates.  NEED TO FIX THIS EVENTUALLY!!!  NUMBERS ARE OLD, CHECK BEFORE REPORT!!!

#Remove NA's  NEED TO FIX THIS EVENTUALLY!!!
Crime <- Crime[!is.na(Crime$X.Coordinate),]

#Import Alley Lights and Street Lights Data w. X and Y coordinates (from Alessandro)
Alley.Lights         <- read.csv(file="alley_lights_out.csv"     , head=TRUE)
Street.Lights.OneOut <- read.csv(file="street-lights-one-out.csv", head=TRUE)
Street.Lights.AllOut <- read.csv(file="street-lights-all-out.csv", head=TRUE)

#Change Date Formats
Alley.Lights$DateCreated           <- as.Date(Alley.Lights$Created.Date          ,format='%d-%b-%y')
Alley.Lights$DateCompleted         <- as.Date(Alley.Lights$Completed.Date        ,format='%d-%b-%y')
Street.Lights.OneOut$DateCreated   <- as.Date(Street.Lights.OneOut$Created.Date  ,format='%d-%b-%y')
Street.Lights.OneOut$DateCompleted <- as.Date(Street.Lights.OneOut$Completed.Date,format='%d-%b-%y')
Street.Lights.AllOut$DateCreated   <- as.Date(Street.Lights.AllOut$Created.Date  ,format='%d-%b-%y')
Street.Lights.AllOut$DateCompleted <- as.Date(Street.Lights.AllOut$Completed.Date,format='%d-%b-%y')

#Keep Only Outages through July 15, 2013, so there is an adequate 'After' Comparison Period
Alley.Lights         <-         Alley.Lights[Alley.Lights$DateCompleted         <= as.Date("7/15/2013", "%m/%d/%Y"),]
Street.Lights.OneOut <- Street.Lights.OneOut[Street.Lights.OneOut$DateCompleted <= as.Date("7/15/2013", "%m/%d/%Y"),]
Street.Lights.AllOut <- Street.Lights.AllOut[Street.Lights.AllOut$DateCompleted <= as.Date("7/15/2013", "%m/%d/%Y"),]

#Generate Outage Duration Variables
Alley.Lights$OutageDuration         <- as.numeric(Alley.Lights$DateCompleted         - Alley.Lights$DateCreated         + 1)
Street.Lights.OneOut$OutageDuration <- as.numeric(Street.Lights.OneOut$DateCompleted - Street.Lights.OneOut$DateCreated + 1)
Street.Lights.AllOut$OutageDuration <- as.numeric(Street.Lights.AllOut$DateCompleted - Street.Lights.AllOut$DateCreated + 1)

#Generate Duration of After Period -- After Comparison Period Won't Always be 30 Days
  #Note, Date of Last Crime in Data is July 30, 2013
for (i in 1:nrow(Alley.Lights))         {Alley.Lights$After.Period.Duration[i]         <- min(30, as.numeric(as.Date("7/30/2013", "%m/%d/%Y")) - as.numeric(Alley.Lights$DateCompleted[i])         - 7)}
for (i in 1:nrow(Street.Lights.OneOut)) {Street.Lights.OneOut$After.Period.Duration[i] <- min(30, as.numeric(as.Date("7/30/2013", "%m/%d/%Y")) - as.numeric(Street.Lights.OneOut$DateCompleted[i]) - 7)}
for (i in 1:nrow(Street.Lights.AllOut)) {Street.Lights.AllOut$After.Period.Duration[i] <- min(30, as.numeric(as.Date("7/30/2013", "%m/%d/%Y")) - as.numeric(Street.Lights.AllOut$DateCompleted[i]) - 7)}


#Loops for Crimes by Outage
#ALLEY LIGHTS
m <- nrow(Alley.Lights)
m
Sys.time()

Thefts.During            <- numeric(m)
Thefts.Before            <- numeric(m)
Thefts.After             <- numeric(m)
Narcotics.During         <- numeric(m)
Narcotics.Before         <- numeric(m)
Narcotics.After          <- numeric(m)
Battery.During           <- numeric(m)
Battery.Before           <- numeric(m)
Battery.After            <- numeric(m)
CriminalDamage.During    <- numeric(m)
CriminalDamage.Before    <- numeric(m)
CriminalDamage.After     <- numeric(m)
MotorVehicleTheft.During <- numeric(m)
MotorVehicleTheft.Before <- numeric(m)
MotorVehicleTheft.After  <- numeric(m)
Robbery.During           <- numeric(m)
Robbery.Before           <- numeric(m)
Robbery.After            <- numeric(m)
Assault.During           <- numeric(m)
Assault.Before           <- numeric(m)
Assault.After            <- numeric(m)
Burglary.During          <- numeric(m)
Burglary.Before          <- numeric(m)
Burglary.After           <- numeric(m)
Homicide.During          <- numeric(m)
Homicide.Before          <- numeric(m)
Homicide.After           <- numeric(m)
DeceptivePractice.During <- numeric(m)
DeceptivePractice.Before <- numeric(m)
DeceptivePractice.After  <- numeric(m)
Crimes.All.During        <- numeric(m)
Crimes.All.Before        <- numeric(m)
Crimes.All.After         <- numeric(m)


for (i in 1:m) {
  Thefts.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                             (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                            as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime) * 
                            as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime) *
                            as.numeric(Crime$Primary.Type == "THEFT")) 
  
  Thefts.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                             (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                            as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime + 37) * 
                            as.numeric(Alley.Lights$DateCreated[i] > Crime$DateCrime + 7) *
                            as.numeric(Crime$Primary.Type == "THEFT")) 
  
  Thefts.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                             (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                            as.numeric(Alley.Lights$DateCompleted[i] < Crime$DateCrime - 7) * 
                            as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime - 37) *
                            as.numeric(Crime$Primary.Type == "THEFT")) 
  
  Narcotics.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                                (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                               as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime) * 
                               as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime) *
                               as.numeric(Crime$Primary.Type == "NARCOTICS")) 
  
  Narcotics.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                                (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                               as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime + 37) * 
                               as.numeric(Alley.Lights$DateCreated[i] > Crime$DateCrime + 7) *
                               as.numeric(Crime$Primary.Type == "NARCOTICS")) 
  
  Narcotics.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                                (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                               as.numeric(Alley.Lights$DateCompleted[i] < Crime$DateCrime - 7) * 
                               as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime - 37) *
                               as.numeric(Crime$Primary.Type == "NARCOTICS")) 
  
  Battery.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                             as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime) * 
                             as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime) *
                             as.numeric(Crime$Primary.Type == "BATTERY")) 
  
  Battery.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                             as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime + 37) * 
                             as.numeric(Alley.Lights$DateCreated[i] > Crime$DateCrime + 7) *
                             as.numeric(Crime$Primary.Type == "BATTERY")) 
  
  Battery.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                             as.numeric(Alley.Lights$DateCompleted[i] < Crime$DateCrime - 7) * 
                             as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime - 37) *
                             as.numeric(Crime$Primary.Type == "BATTERY")) 
  
  CriminalDamage.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                                     (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                                    as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime) * 
                                    as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime) *
                                    as.numeric(Crime$Primary.Type == "CRIMINAL DAMAGE")) 
  
  CriminalDamage.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                                     (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                                    as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime + 37) * 
                                    as.numeric(Alley.Lights$DateCreated[i] > Crime$DateCrime + 7) *
                                    as.numeric(Crime$Primary.Type == "CRIMINAL DAMAGE")) 
  
  CriminalDamage.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                                     (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                                    as.numeric(Alley.Lights$DateCompleted[i] < Crime$DateCrime - 7) * 
                                    as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime - 37) *
                                    as.numeric(Crime$Primary.Type == "CRIMINAL DAMAGE")) 
  
  MotorVehicleTheft.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                                        (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                                       as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime) * 
                                       as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime) *
                                       as.numeric(Crime$Primary.Type == "MOTOR VEHICLE THEFT")) 
  
  MotorVehicleTheft.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                                        (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                                       as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime + 37) * 
                                       as.numeric(Alley.Lights$DateCreated[i] > Crime$DateCrime + 7) *
                                       as.numeric(Crime$Primary.Type == "MOTOR VEHICLE THEFT")) 
  
  MotorVehicleTheft.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                                        (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                                       as.numeric(Alley.Lights$DateCompleted[i] < Crime$DateCrime - 7) * 
                                       as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime - 37) *
                                       as.numeric(Crime$Primary.Type == "MOTOR VEHICLE THEFT")) 
  
  Robbery.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                             as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime) * 
                             as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime) *
                             as.numeric(Crime$Primary.Type == "ROBBERY")) 
  
  Robbery.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                             as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime + 37) * 
                             as.numeric(Alley.Lights$DateCreated[i] > Crime$DateCrime + 7) *
                             as.numeric(Crime$Primary.Type == "ROBBERY")) 
  
  Robbery.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                             as.numeric(Alley.Lights$DateCompleted[i] < Crime$DateCrime - 7) * 
                             as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime - 37) *
                             as.numeric(Crime$Primary.Type == "ROBBERY")) 
  
  Assault.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                             as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime) * 
                             as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime) *
                             as.numeric(Crime$Primary.Type == "ASSAULT")) 
  
  Assault.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                             as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime + 37) * 
                             as.numeric(Alley.Lights$DateCreated[i] > Crime$DateCrime + 7) *
                             as.numeric(Crime$Primary.Type == "ASSAULT")) 
  
  Assault.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                             as.numeric(Alley.Lights$DateCompleted[i] < Crime$DateCrime - 7) * 
                             as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime - 37) *
                             as.numeric(Crime$Primary.Type == "ASSAULT")) 
  
  Burglary.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                              as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime) * 
                              as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime) *
                              as.numeric(Crime$Primary.Type == "BURGLARY")) 
  
  Burglary.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                              as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime + 37) * 
                              as.numeric(Alley.Lights$DateCreated[i] > Crime$DateCrime + 7) *
                              as.numeric(Crime$Primary.Type == "BURGLARY")) 
  
  Burglary.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                              as.numeric(Alley.Lights$DateCompleted[i] < Crime$DateCrime - 7) * 
                              as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime - 37) *
                              as.numeric(Crime$Primary.Type == "BURGLARY")) 
  
  Homicide.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                              as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime) * 
                              as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime) *
                              as.numeric(Crime$Primary.Type == "HOMICIDE")) 
  
  Homicide.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                              as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime + 37) * 
                              as.numeric(Alley.Lights$DateCreated[i] > Crime$DateCrime + 7) *
                              as.numeric(Crime$Primary.Type == "HOMICIDE")) 
  
  Homicide.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                              as.numeric(Alley.Lights$DateCompleted[i] < Crime$DateCrime - 7) * 
                              as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime - 37) *
                              as.numeric(Crime$Primary.Type == "HOMICIDE")) 

  DeceptivePractice.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                              as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime) * 
                              as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime) *
                              as.numeric(Crime$Primary.Type == "DECEPTIVE PRACTICE")) 
  
  DeceptivePractice.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                              as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime + 37) * 
                              as.numeric(Alley.Lights$DateCreated[i] > Crime$DateCrime + 7) *
                              as.numeric(Crime$Primary.Type == "DECEPTIVE PRACTICE")) 
  
  DeceptivePractice.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                              as.numeric(Alley.Lights$DateCompleted[i] < Crime$DateCrime - 7) * 
                              as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime - 37) *
                              as.numeric(Crime$Primary.Type == "DECEPTIVE PRACTICE")) 
  
  Crimes.All.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                                as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime) * 
                                as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime) *
                                as.numeric(Crime$Primary.Type != "DECEPTIVE PRACTICE")) 
 

  Crimes.All.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                                 (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                                as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime + 37) * 
                                as.numeric(Alley.Lights$DateCreated[i] > Crime$DateCrime + 7) *
                                as.numeric(Crime$Primary.Type != "DECEPTIVE PRACTICE")) 
 
  
  Crimes.All.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                                 (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                                as.numeric(Alley.Lights$DateCompleted[i] < Crime$DateCrime - 7) * 
                                as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime - 37) *
                                as.numeric(Crime$Primary.Type != "DECEPTIVE PRACTICE")) 

  
  if(i%%100==0)  cat("loop", i, "\n")
}

Sys.time()

Alley.Lights$Thefts.During            <- Thefts.During 
Alley.Lights$Thefts.Before            <- Thefts.Before
Alley.Lights$Thefts.After             <- Thefts.After
Alley.Lights$Narcotics.During         <- Narcotics.During 
Alley.Lights$Narcotics.Before         <- Narcotics.Before
Alley.Lights$Narcotics.After          <- Narcotics.After
Alley.Lights$Battery.During           <- Battery.During 
Alley.Lights$Battery.Before           <- Battery.Before
Alley.Lights$Battery.After            <- Battery.After
Alley.Lights$CriminalDamage.During    <- CriminalDamage.During 
Alley.Lights$CriminalDamage.Before    <- CriminalDamage.Before
Alley.Lights$CriminalDamage.After     <- CriminalDamage.After
Alley.Lights$MotorVehicleTheft.During <- MotorVehicleTheft.During 
Alley.Lights$MotorVehicleTheft.Before <- MotorVehicleTheft.Before
Alley.Lights$MotorVehicleTheft.After  <- MotorVehicleTheft.After
Alley.Lights$Robbery.During           <- Robbery.During 
Alley.Lights$Robbery.Before           <- Robbery.Before
Alley.Lights$Robbery.After            <- Robbery.After
Alley.Lights$Assault.During           <- Assault.During 
Alley.Lights$Assault.Before           <- Assault.Before
Alley.Lights$Assault.After            <- Assault.After
Alley.Lights$Burglary.During          <- Burglary.During 
Alley.Lights$Burglary.Before          <- Burglary.Before
Alley.Lights$Burglary.After           <- Burglary.After
Alley.Lights$Homicide.During          <- Homicide.During 
Alley.Lights$Homicide.Before          <- Homicide.Before
Alley.Lights$Homicide.After           <- Homicide.After
Alley.Lights$DeceptivePractice.During <- DeceptivePractice.During 
Alley.Lights$DeceptivePractice.Before <- DeceptivePractice.Before
Alley.Lights$DeceptivePractice.After  <- DeceptivePractice.After
Alley.Lights$Crimes.All.During        <- Crimes.All.During 
Alley.Lights$Crimes.All.Before        <- Crimes.All.Before
Alley.Lights$Crimes.All.After         <- Crimes.All.After

#EXPORT DATA SET
write.csv(Alley.Lights, file="Alley_Lights_and_Crime.csv")


#Loop -- Street Lights - One Out
m <- nrow(Street.Lights.OneOut)
m
Sys.time()
Thefts.During            <- numeric(m)
Thefts.Before            <- numeric(m)
Thefts.After             <- numeric(m)
Narcotics.During         <- numeric(m)
Narcotics.Before         <- numeric(m)
Narcotics.After          <- numeric(m)
Battery.During           <- numeric(m)
Battery.Before           <- numeric(m)
Battery.After            <- numeric(m)
CriminalDamage.During    <- numeric(m)
CriminalDamage.Before    <- numeric(m)
CriminalDamage.After     <- numeric(m)
MotorVehicleTheft.During <- numeric(m)
MotorVehicleTheft.Before <- numeric(m)
MotorVehicleTheft.After  <- numeric(m)
Robbery.During           <- numeric(m)
Robbery.Before           <- numeric(m)
Robbery.After            <- numeric(m)
Assault.During           <- numeric(m)
Assault.Before           <- numeric(m)
Assault.After            <- numeric(m)
Burglary.During          <- numeric(m)
Burglary.Before          <- numeric(m)
Burglary.After           <- numeric(m)
Homicide.During          <- numeric(m)
Homicide.Before          <- numeric(m)
Homicide.After           <- numeric(m)
DeceptivePractice.During <- numeric(m)
DeceptivePractice.Before <- numeric(m)
DeceptivePractice.After  <- numeric(m)
Crimes.All.During        <- numeric(m)
Crimes.All.Before        <- numeric(m)
Crimes.All.After         <- numeric(m)

for (i in 1:m) {
  Thefts.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                             (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                            as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime) * 
                            as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime) *
                            as.numeric(Crime$Primary.Type == "THEFT")) 
  
  Thefts.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                             (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                            as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                            as.numeric(Street.Lights.OneOut$DateCreated[i] > Crime$DateCrime + 7) *
                            as.numeric(Crime$Primary.Type == "THEFT")) 
  
  Thefts.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                             (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                            as.numeric(Street.Lights.OneOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                            as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                            as.numeric(Crime$Primary.Type == "THEFT")) 
  
  Narcotics.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                                (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                               as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime) * 
                               as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime) *
                               as.numeric(Crime$Primary.Type == "NARCOTICS")) 
  
  Narcotics.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                                (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                               as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                               as.numeric(Street.Lights.OneOut$DateCreated[i] > Crime$DateCrime + 7) *
                               as.numeric(Crime$Primary.Type == "NARCOTICS")) 
  
  Narcotics.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                                (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                               as.numeric(Street.Lights.OneOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                               as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                               as.numeric(Crime$Primary.Type == "NARCOTICS")) 
  
  Battery.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                             as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime) * 
                             as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime) *
                             as.numeric(Crime$Primary.Type == "BATTERY")) 
  
  Battery.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                             as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                             as.numeric(Street.Lights.OneOut$DateCreated[i] > Crime$DateCrime + 7) *
                             as.numeric(Crime$Primary.Type == "BATTERY")) 
  
  Battery.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                             as.numeric(Street.Lights.OneOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                             as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                             as.numeric(Crime$Primary.Type == "BATTERY")) 
  
  CriminalDamage.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                                     (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                                    as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime) * 
                                    as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime) *
                                    as.numeric(Crime$Primary.Type == "CRIMINAL DAMAGE")) 
  
  CriminalDamage.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                                     (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                                    as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                                    as.numeric(Street.Lights.OneOut$DateCreated[i] > Crime$DateCrime + 7) *
                                    as.numeric(Crime$Primary.Type == "CRIMINAL DAMAGE")) 
  
  CriminalDamage.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                                     (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                                    as.numeric(Street.Lights.OneOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                                    as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                                    as.numeric(Crime$Primary.Type == "CRIMINAL DAMAGE")) 
  
  MotorVehicleTheft.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                                        (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                                       as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime) * 
                                       as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime) *
                                       as.numeric(Crime$Primary.Type == "MOTOR VEHICLE THEFT")) 
  
  MotorVehicleTheft.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                                        (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                                       as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                                       as.numeric(Street.Lights.OneOut$DateCreated[i] > Crime$DateCrime + 7) *
                                       as.numeric(Crime$Primary.Type == "MOTOR VEHICLE THEFT")) 
  
  MotorVehicleTheft.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                                        (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                                       as.numeric(Street.Lights.OneOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                                       as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                                       as.numeric(Crime$Primary.Type == "MOTOR VEHICLE THEFT")) 
  
  Robbery.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                             as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime) * 
                             as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime) *
                             as.numeric(Crime$Primary.Type == "ROBBERY")) 
  
  Robbery.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                             as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                             as.numeric(Street.Lights.OneOut$DateCreated[i] > Crime$DateCrime + 7) *
                             as.numeric(Crime$Primary.Type == "ROBBERY")) 
  
  Robbery.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                             as.numeric(Street.Lights.OneOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                             as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                             as.numeric(Crime$Primary.Type == "ROBBERY")) 
  
  Assault.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                             as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime) * 
                             as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime) *
                             as.numeric(Crime$Primary.Type == "ASSAULT")) 
  
  Assault.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                             as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                             as.numeric(Street.Lights.OneOut$DateCreated[i] > Crime$DateCrime + 7) *
                             as.numeric(Crime$Primary.Type == "ASSAULT")) 
  
  Assault.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                             as.numeric(Street.Lights.OneOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                             as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                             as.numeric(Crime$Primary.Type == "ASSAULT")) 
  
  Burglary.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                              as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime) * 
                              as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime) *
                              as.numeric(Crime$Primary.Type == "BURGLARY")) 
  
  Burglary.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                              as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                              as.numeric(Street.Lights.OneOut$DateCreated[i] > Crime$DateCrime + 7) *
                              as.numeric(Crime$Primary.Type == "BURGLARY")) 
  
  Burglary.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                              as.numeric(Street.Lights.OneOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                              as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                              as.numeric(Crime$Primary.Type == "BURGLARY")) 
  
  Homicide.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                              as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime) * 
                              as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime) *
                              as.numeric(Crime$Primary.Type == "HOMICIDE")) 
  
  Homicide.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                              as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                              as.numeric(Street.Lights.OneOut$DateCreated[i] > Crime$DateCrime + 7) *
                              as.numeric(Crime$Primary.Type == "HOMICIDE")) 
  
  Homicide.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                              as.numeric(Street.Lights.OneOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                              as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                              as.numeric(Crime$Primary.Type == "HOMICIDE")) 

  DeceptivePractice.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                              as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime) * 
                              as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime) *
                              as.numeric(Crime$Primary.Type == "DECEPTIVE PRACTICE")) 
  
  DeceptivePractice.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                              as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                              as.numeric(Street.Lights.OneOut$DateCreated[i] > Crime$DateCrime + 7) *
                              as.numeric(Crime$Primary.Type == "DECEPTIVE PRACTICE")) 
  
  DeceptivePractice.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                              as.numeric(Street.Lights.OneOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                              as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                              as.numeric(Crime$Primary.Type == "DECEPTIVE PRACTICE")) 
  
  Crimes.All.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                                 (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                                as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime) * 
                                as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime) *
                                as.numeric(Crime$Primary.Type != "DECEPTIVE PRACTICE")) 
 
  
  Crimes.All.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                                 (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                                as.numeric(Street.Lights.OneOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                                as.numeric(Street.Lights.OneOut$DateCreated[i] > Crime$DateCrime + 7) *
                                as.numeric(Crime$Primary.Type != "DECEPTIVE PRACTICE")) 
 
  
  Crimes.All.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.OneOut$x_coord[i])**2 + 
                                                 (Crime$Y.Coordinate-Street.Lights.OneOut$y_coord[i])**2) < 500) * 
                                as.numeric(Street.Lights.OneOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                                as.numeric(Street.Lights.OneOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                                as.numeric(Crime$Primary.Type != "DECEPTIVE PRACTICE")) 

  
  if(i%%100==0)  cat("loop", i, "\n")
}

Sys.time()

Street.Lights.OneOut$Thefts.During            <- Thefts.During 
Street.Lights.OneOut$Thefts.Before            <- Thefts.Before
Street.Lights.OneOut$Thefts.After             <- Thefts.After
Street.Lights.OneOut$Narcotics.During         <- Narcotics.During 
Street.Lights.OneOut$Narcotics.Before         <- Narcotics.Before
Street.Lights.OneOut$Narcotics.After          <- Narcotics.After
Street.Lights.OneOut$Battery.During           <- Battery.During 
Street.Lights.OneOut$Battery.Before           <- Battery.Before
Street.Lights.OneOut$Battery.After            <- Battery.After
Street.Lights.OneOut$CriminalDamage.During    <- CriminalDamage.During 
Street.Lights.OneOut$CriminalDamage.Before    <- CriminalDamage.Before
Street.Lights.OneOut$CriminalDamage.After     <- CriminalDamage.After
Street.Lights.OneOut$MotorVehicleTheft.During <- MotorVehicleTheft.During 
Street.Lights.OneOut$MotorVehicleTheft.Before <- MotorVehicleTheft.Before
Street.Lights.OneOut$MotorVehicleTheft.After  <- MotorVehicleTheft.After
Street.Lights.OneOut$Robbery.During           <- Robbery.During 
Street.Lights.OneOut$Robbery.Before           <- Robbery.Before
Street.Lights.OneOut$Robbery.After            <- Robbery.After
Street.Lights.OneOut$Assault.During           <- Assault.During 
Street.Lights.OneOut$Assault.Before           <- Assault.Before
Street.Lights.OneOut$Assault.After            <- Assault.After
Street.Lights.OneOut$Burglary.During          <- Burglary.During 
Street.Lights.OneOut$Burglary.Before          <- Burglary.Before
Street.Lights.OneOut$Burglary.After           <- Burglary.After
Street.Lights.OneOut$Homicide.During          <- Homicide.During 
Street.Lights.OneOut$Homicide.Before          <- Homicide.Before
Street.Lights.OneOut$Homicide.After           <- Homicide.After
Street.Lights.OneOut$DeceptivePractice.During <- DeceptivePractice.During 
Street.Lights.OneOut$DeceptivePractice.Before <- DeceptivePractice.Before
Street.Lights.OneOut$DeceptivePractice.After  <- DeceptivePractice.After
Street.Lights.OneOut$Crimes.All.During        <- Crimes.All.During 
Street.Lights.OneOut$Crimes.All.Before        <- Crimes.All.Before
Street.Lights.OneOut$Crimes.All.After         <- Crimes.All.After

#EXPORT DATA SET
write.csv(Street.Lights.OneOut, file="Street_Lights_One_Out_and_Crime.csv")


#Loop -- Street Lights - All Out
m <- nrow(Street.Lights.AllOut)
m
Sys.time()
Thefts.During            <- numeric(m)
Thefts.Before            <- numeric(m)
Thefts.After             <- numeric(m)
Narcotics.During         <- numeric(m)
Narcotics.Before         <- numeric(m)
Narcotics.After          <- numeric(m)
Battery.During           <- numeric(m)
Battery.Before           <- numeric(m)
Battery.After            <- numeric(m)
CriminalDamage.During    <- numeric(m)
CriminalDamage.Before    <- numeric(m)
CriminalDamage.After     <- numeric(m)
MotorVehicleTheft.During <- numeric(m)
MotorVehicleTheft.Before <- numeric(m)
MotorVehicleTheft.After  <- numeric(m)
Robbery.During           <- numeric(m)
Robbery.Before           <- numeric(m)
Robbery.After            <- numeric(m)
Assault.During           <- numeric(m)
Assault.Before           <- numeric(m)
Assault.After            <- numeric(m)
Burglary.During          <- numeric(m)
Burglary.Before          <- numeric(m)
Burglary.After           <- numeric(m)
Homicide.During          <- numeric(m)
Homicide.Before          <- numeric(m)
Homicide.After           <- numeric(m)
DeceptivePractice.During <- numeric(m)
DeceptivePractice.Before <- numeric(m)
DeceptivePractice.After  <- numeric(m)
Crimes.All.During        <- numeric(m)
Crimes.All.Before        <- numeric(m)
Crimes.All.After         <- numeric(m)

for (i in 1:m) {
  Thefts.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                             (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                            as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime) * 
                            as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime) *
                            as.numeric(Crime$Primary.Type == "THEFT")) 
  
  Thefts.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                             (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                            as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                            as.numeric(Street.Lights.AllOut$DateCreated[i] > Crime$DateCrime + 7) *
                            as.numeric(Crime$Primary.Type == "THEFT")) 
  
  Thefts.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                             (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                            as.numeric(Street.Lights.AllOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                            as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                            as.numeric(Crime$Primary.Type == "THEFT")) 
  
  Narcotics.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                                (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                               as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime) * 
                               as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime) *
                               as.numeric(Crime$Primary.Type == "NARCOTICS")) 
  
  Narcotics.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                                (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                               as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                               as.numeric(Street.Lights.AllOut$DateCreated[i] > Crime$DateCrime + 7) *
                               as.numeric(Crime$Primary.Type == "NARCOTICS")) 
  
  Narcotics.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                                (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                               as.numeric(Street.Lights.AllOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                               as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                               as.numeric(Crime$Primary.Type == "NARCOTICS")) 
  
  Battery.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                             as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime) * 
                             as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime) *
                             as.numeric(Crime$Primary.Type == "BATTERY")) 
  
  Battery.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                             as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                             as.numeric(Street.Lights.AllOut$DateCreated[i] > Crime$DateCrime + 7) *
                             as.numeric(Crime$Primary.Type == "BATTERY")) 
  
  Battery.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                             as.numeric(Street.Lights.AllOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                             as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                             as.numeric(Crime$Primary.Type == "BATTERY")) 
  
  CriminalDamage.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                                     (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                                    as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime) * 
                                    as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime) *
                                    as.numeric(Crime$Primary.Type == "CRIMINAL DAMAGE")) 
  
  CriminalDamage.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                                     (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                                    as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                                    as.numeric(Street.Lights.AllOut$DateCreated[i] > Crime$DateCrime + 7) *
                                    as.numeric(Crime$Primary.Type == "CRIMINAL DAMAGE")) 
  
  CriminalDamage.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                                     (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                                    as.numeric(Street.Lights.AllOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                                    as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                                    as.numeric(Crime$Primary.Type == "CRIMINAL DAMAGE")) 
  
  MotorVehicleTheft.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                                        (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                                       as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime) * 
                                       as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime) *
                                       as.numeric(Crime$Primary.Type == "MOTOR VEHICLE THEFT")) 
  
  MotorVehicleTheft.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                                        (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                                       as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                                       as.numeric(Street.Lights.AllOut$DateCreated[i] > Crime$DateCrime + 7) *
                                       as.numeric(Crime$Primary.Type == "MOTOR VEHICLE THEFT")) 
  
  MotorVehicleTheft.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                                        (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                                       as.numeric(Street.Lights.AllOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                                       as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                                       as.numeric(Crime$Primary.Type == "MOTOR VEHICLE THEFT")) 
  
  Robbery.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                             as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime) * 
                             as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime) *
                             as.numeric(Crime$Primary.Type == "ROBBERY")) 
  
  Robbery.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                             as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                             as.numeric(Street.Lights.AllOut$DateCreated[i] > Crime$DateCrime + 7) *
                             as.numeric(Crime$Primary.Type == "ROBBERY")) 
  
  Robbery.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                             as.numeric(Street.Lights.AllOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                             as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                             as.numeric(Crime$Primary.Type == "ROBBERY")) 
  
  Assault.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                             as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime) * 
                             as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime) *
                             as.numeric(Crime$Primary.Type == "ASSAULT")) 
  
  Assault.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                             as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                             as.numeric(Street.Lights.AllOut$DateCreated[i] > Crime$DateCrime + 7) *
                             as.numeric(Crime$Primary.Type == "ASSAULT")) 
  
  Assault.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                              (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                             as.numeric(Street.Lights.AllOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                             as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                             as.numeric(Crime$Primary.Type == "ASSAULT")) 
  
  Burglary.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                              as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime) * 
                              as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime) *
                              as.numeric(Crime$Primary.Type == "BURGLARY")) 
  
  Burglary.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                              as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                              as.numeric(Street.Lights.AllOut$DateCreated[i] > Crime$DateCrime + 7) *
                              as.numeric(Crime$Primary.Type == "BURGLARY")) 
  
  Burglary.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                              as.numeric(Street.Lights.AllOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                              as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                              as.numeric(Crime$Primary.Type == "BURGLARY")) 
  
  Homicide.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                              as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime) * 
                              as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime) *
                              as.numeric(Crime$Primary.Type == "HOMICIDE")) 
  
  Homicide.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                              as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                              as.numeric(Street.Lights.AllOut$DateCreated[i] > Crime$DateCrime + 7) *
                              as.numeric(Crime$Primary.Type == "HOMICIDE")) 
  
  Homicide.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                              as.numeric(Street.Lights.AllOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                              as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                              as.numeric(Crime$Primary.Type == "HOMICIDE")) 

  DeceptivePractice.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                              as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime) * 
                              as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime) *
                              as.numeric(Crime$Primary.Type == "DECEPTIVE PRACTICE")) 
  
  DeceptivePractice.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                              as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                              as.numeric(Street.Lights.AllOut$DateCreated[i] > Crime$DateCrime + 7) *
                              as.numeric(Crime$Primary.Type == "DECEPTIVE PRACTICE")) 
  
  DeceptivePractice.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                              as.numeric(Street.Lights.AllOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                              as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                              as.numeric(Crime$Primary.Type == "DECEPTIVE PRACTICE")) 
  
  Crimes.All.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                                 (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                              as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime) *
				  as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime) *
                              as.numeric(Crime$Primary.Type != "DECEPTIVE PRACTICE")) 
 
  
  Crimes.All.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                                 (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                                as.numeric(Street.Lights.AllOut$DateCreated[i] <= Crime$DateCrime + 37) * 
                                as.numeric(Street.Lights.AllOut$DateCreated[i] > Crime$DateCrime + 7) *
                                as.numeric(Crime$Primary.Type != "DECEPTIVE PRACTICE")) 
 
  
  Crimes.All.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Street.Lights.AllOut$x_coord[i])**2 + 
                                                 (Crime$Y.Coordinate-Street.Lights.AllOut$y_coord[i])**2) < 1000) * 
                                as.numeric(Street.Lights.AllOut$DateCompleted[i] < Crime$DateCrime - 7) * 
                                as.numeric(Street.Lights.AllOut$DateCompleted[i] >= Crime$DateCrime - 37) *
                                as.numeric(Crime$Primary.Type != "DECEPTIVE PRACTICE")) 
  
  if(i%%100==0)  cat("loop", i, "\n")
}

Sys.time()

Street.Lights.AllOut$Thefts.During            <- Thefts.During 
Street.Lights.AllOut$Thefts.Before            <- Thefts.Before
Street.Lights.AllOut$Thefts.After             <- Thefts.After
Street.Lights.AllOut$Narcotics.During         <- Narcotics.During 
Street.Lights.AllOut$Narcotics.Before         <- Narcotics.Before
Street.Lights.AllOut$Narcotics.After          <- Narcotics.After
Street.Lights.AllOut$Battery.During           <- Battery.During 
Street.Lights.AllOut$Battery.Before           <- Battery.Before
Street.Lights.AllOut$Battery.After            <- Battery.After
Street.Lights.AllOut$CriminalDamage.During    <- CriminalDamage.During 
Street.Lights.AllOut$CriminalDamage.Before    <- CriminalDamage.Before
Street.Lights.AllOut$CriminalDamage.After     <- CriminalDamage.After
Street.Lights.AllOut$MotorVehicleTheft.During <- MotorVehicleTheft.During 
Street.Lights.AllOut$MotorVehicleTheft.Before <- MotorVehicleTheft.Before
Street.Lights.AllOut$MotorVehicleTheft.After  <- MotorVehicleTheft.After
Street.Lights.AllOut$Robbery.During           <- Robbery.During 
Street.Lights.AllOut$Robbery.Before           <- Robbery.Before
Street.Lights.AllOut$Robbery.After            <- Robbery.After
Street.Lights.AllOut$Assault.During           <- Assault.During 
Street.Lights.AllOut$Assault.Before           <- Assault.Before
Street.Lights.AllOut$Assault.After            <- Assault.After
Street.Lights.AllOut$Burglary.During          <- Burglary.During 
Street.Lights.AllOut$Burglary.Before          <- Burglary.Before
Street.Lights.AllOut$Burglary.After           <- Burglary.After
Street.Lights.AllOut$Homicide.During          <- Homicide.During 
Street.Lights.AllOut$Homicide.Before          <- Homicide.Before
Street.Lights.AllOut$Homicide.After           <- Homicide.After
Street.Lights.AllOut$DeceptivePractice.During <- DeceptivePractice.During 
Street.Lights.AllOut$DeceptivePractice.Before <- DeceptivePractice.Before
Street.Lights.AllOut$DeceptivePractice.After  <- DeceptivePractice.After
Street.Lights.AllOut$Crimes.All.During        <- Crimes.All.During 
Street.Lights.AllOut$Crimes.All.Before        <- Crimes.All.Before
Street.Lights.AllOut$Crimes.All.After         <- Crimes.All.After

#EXPORT DATA SET
write.csv(Street.Lights.AllOut, file="Street_Lights_All_Out_and_Crime.csv")

save.image()





