###Lights Seasonality Graph

lights<-read.csv("",header=T)
lights$Date <- as.POSIXlt(strptime(lights$Created.Date, format="%d-%B-%y"))
lights$Month <- format(lights$Date,"%m")
lights$Year<-format(lights$Date,"%y")
lights$Year_Month<-paste("20",lights$Year,"-",lights$Month,"-","01",sep="")
lights$Count <- 1

lights_month<- aggregate(lights$Count,list(lights$Year_Month),sum)
colnames(lights_month)<-c("Month_Year","Lights_Count")
lights_month$Month_Year<-as.Date(lights_month$Month_Year,format="%Y-%m-%d")

library(ggplot2)
image1<- qplot(lights_month$Month_Year[-16],lights_month$Lights_Count[-16])+
        geom_line()+
        ylab("Total Number of Lights Out")+
        xlab("Month")+
        ylim(250,1500)+
        ggtitle("CDoT One Light Out")

#open lights
openlights<-read.csv("",header=T)
openlights$Date<- as.POSIXlt(strptime(openlights$Creation.Date, format="%m/%d/%Y"))
openlights$Month <- format(openlights$Date,"%m")
openlights$Year<-format(openlights$Date,"%y")
openlights$Year_Month<-paste("20",openlights$Year,"-",openlights$Month,"-","01",sep="")
openlights$Count <- 1

openlights_month<- aggregate(openlights$Count,list(openlights$Year_Month),sum)
colnames(openlights_month)<-c("Month_Year","Lights_Count")
openlights_month$Month_Year<-as.Date(openlights_month$Month_Year,format="%Y-%m-%d")

mean(openlights_month$Lights_Count)*16

library(ggplot2)
image2<- qplot(openlights_month$Month_Year,openlights_month$Lights_Count)+
  geom_line()+
  ylab("Total Number of Lights Out")+
  xlab("Month")+
  xlim(as.Date("2012-04-01",format="%Y-%m-%d"),
       as.Date("2013-08-01",format="%Y-%m-%d"))+
  ggtitle("Open Portal One Light Out")

###find uniques
length(which(openlights$Date>as.POSIXlt("2012-04-01",format="%Y-%m-%d")&
      openlights$Date<as.POSIXlt("2013-08-01",format="%Y-%m-%d")))

length(which(duplicated(openlights$Service.Request.Number)=="TRUE"))

length(which(duplicated(openlights$Location)=="TRUE"))

length(which(duplicated(lights$Location)=="TRUE"))


###no location dupes/no incompletes

lilights <- lights[which(duplicated(lights$Location)=="FALSE"),]


lilights_month<- aggregate(lilights$Count,list(lilights$Year_Month),sum)
colnames(lilights_month)<-c("Month_Year","Lights_Count")
lilights_month$Month_Year<-as.Date(lilights_month$Month_Year,format="%Y-%m-%d")

lilopenlights <-openlights[which(duplicated(openlights$Location)=="FALSE"),]
lilopenlights <- lilopenlights[which(lilopenlights$Status=="Completed"),]

lilopenlights_month<- aggregate(lilopenlights$Count,list(lilopenlights$Year_Month),sum)
colnames(lilopenlights_month)<-c("Month_Year","Lights_Count")
lilopenlights_month$Month_Year<-as.Date(lilopenlights_month$Month_Year,format="%Y-%m-%d")

library(ggplot2)
lilimage1<- qplot(lilights_month$Month_Year[-16],lilights_month$Lights_Count[-16])+
  geom_line()+
  ylab("Total Number of Lights Out")+
  xlab("Month")+
  ylim(0,1500)+
  ggtitle("CDoT One Light Out No Dups")

library(ggplot2)
lilimage2<- qplot(lilopenlights_month$Month_Year,lilopenlights_month$Lights_Count)+
  geom_line()+
  ylab("Total Number of Lights Out")+
  xlab("Month")+
  xlim(as.Date("2012-04-01",format="%Y-%m-%d"),
       as.Date("2013-08-01",format="%Y-%m-%d"))+
  ggtitle("Open Portal One Lights Out No Dups")+
  ylim(0,1500)

library(gridExtra)
grid.arrange(lilimage1, lilimage2, ncol=2)

setwd("")
ggsave(file="", plot=lilimage1, width=10, height=8)



