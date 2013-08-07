rm(list = ls())

setwd("C:/Users/Zach/Documents/UrbanCCD/Streetlights")

#Open Crime Portal Data
Crime <- read.csv(file="Crimes_-_Feb_15__2012_to_August_6__2013_-_Outdoor.csv", head=TRUE)
crime.type.tab <- as.data.frame(summary(Crime$Primary.Type))
crime.location.tab <- as.data.frame(summary(Crime$Location.Description))

#Keep only Crime Types of Interest
Crime <- Crime[Crime$Primary.Type %in% c("THEFT", "NARCOTICS", "BATTERY", "CRIMINAL DAMAGE", "MOTOR VEHICLE THEFT", "ROBBERY", "ASSAULT", "BURGLARY", "HOMICIDE"),]
crime.type.tab.2 <- as.data.frame(summary(Crime$Primary.Type))

#Create New Date Variable for Crime
Crime$DateCrime <- as.Date(substr(as.character(Crime$Date), 1, nchar(as.character(Crime$Date))-5), "%m/%d/%Y")

#Check # of NA's in Crime Location Data
summary(Crime$X.Coordinate)
#4446 out of 210898 have no X and Y coordinates.  NEED TO FIX THIS EVENTUALLY!!!

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


#Toy Datasets to Try Loop
Alley.Toy <- Alley.Lights[1:100,]
Crime.Toy <- Crime[1:200,]

#Toy Loop
m <- nrow(Alley.Toy)
Alley.Toy$NumberCrimes <- rep(0,m)


for (i in 1:m) {
  Alley.Toy$NumberCrimes[i] <- sum(as.numeric( sqrt((Crime.Toy$X.Coordinate-Alley.Toy$x_coord[i])**2 + 
                                                    (Crime.Toy$Y.Coordinate-Alley.Toy$y_coord[i])**2) < 500) * 
                                   as.numeric(Alley.Toy$DateCreated[i] <= Crime.Toy$DateCrime) * 
                                   as.numeric(Alley.Toy$DateCompleted[i] >= Crime.Toy$DateCrime)) 

}



#Real Loop
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
Crimes.Alley.During      <- numeric(m)
Crimes.Alley.Before      <- numeric(m)
Crimes.Alley.After       <- numeric(m)
Crimes.All.During        <- numeric(m)
Crimes.All.During.NoDay1 <- numeric(m)
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
  
  Crimes.Alley.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                                (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                                as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime) * 
                                as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime) *
                                as.numeric(Crime$Location.Description == "ALLEY")) 
  
  Crimes.Alley.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                                (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                                as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime + 37) * 
                                as.numeric(Alley.Lights$DateCreated[i] > Crime$DateCrime + 7) *
                                as.numeric(Crime$Location.Description == "ALLEY")) 
  
  Crimes.Alley.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                                 (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                                as.numeric(Alley.Lights$DateCompleted[i] < Crime$DateCrime - 7) * 
                                as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime - 37) *
                                as.numeric(Crime$Location.Description == "ALLEY")) 
  
  Crimes.All.During[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                              as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime) * 
                              as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime)) 
  
  Crimes.All.During.NoDay1[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                                      (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                                     as.numeric(Alley.Lights$DateCreated[i] < Crime$DateCrime) * 
                                     as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime)) 
  
  Crimes.All.Before[i] <- sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                              as.numeric(Alley.Lights$DateCreated[i] <= Crime$DateCrime + 37) * 
                              as.numeric(Alley.Lights$DateCreated[i] > Crime$DateCrime + 7) 
  
  Crimes.All.After[i] <-  sum(as.numeric( sqrt((Crime$X.Coordinate-Alley.Lights$x_coord[i])**2 + 
                                               (Crime$Y.Coordinate-Alley.Lights$y_coord[i])**2) < 500) * 
                              as.numeric(Alley.Lights$DateCompleted[i] < Crime$DateCrime - 7) * 
                              as.numeric(Alley.Lights$DateCompleted[i] >= Crime$DateCrime - 37)

  if(i%%250==0)  cat("loop", i, "\n")
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
Alley.Lights$Crimes.Alley.During      <- Crimes.Alley.During 
Alley.Lights$Crimes.Alley.Before      <- Crimes.Alley.Before
Alley.Lights$Crimes.Alley.After       <- Crimes.Alley.After

#EXPORT DATA SETS

# Extra Variables -- Can Be Sent to Pt 2 Program
# Alley.Lights$CrimeRateDuring <- 30*Alley.Lights$NumberCrimesDuring/as.numeric(Alley.Lights$DateCompleted - Alley.Lights$DateCreated + 1)





