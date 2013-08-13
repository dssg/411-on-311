###Crime Seasonality Graph

crime<-read.csv("C:/Users/dssg-Indices-project/streetlights_crime/data/All_Crimes__April_2012-July_2013.csv",header=T)
crime$Date <- as.POSIXlt(strptime(crime$Date, format="%m/%d/%Y %H:%M"))
crime$Month <- format(crime$Date,"%m")
crime$Year<-format(crime$Date,"%y")
crime$Year_Month<-paste("20",crime$Year,"-",crime$Month,"-","01",sep="")
crime$Count <- 1

crime_month<- aggregate(crime$Count,list(crime$Year_Month),sum)
colnames(crime_month)<-c("Month_Year","Crime_Count")
crime_month$Month_Year<-as.Date(crime_month$Month_Year,format="%Y-%m-%d")


image<- qplot(crime_month$Month_Year[-16],crime_month$Crime_Count[-16])+
        geom_line()+
        ylab("Total Number of Crimes")+
        xlab("Month")

image
setwd("C:/Users/Desktop")
ggsave(file="Crime_Seasonality_Graph.jpeg", plot=image, width=10, height=8)
