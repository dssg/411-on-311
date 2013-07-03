rm(list = ls())
setwd("C:/Users/Zach/Documents/UrbanCCD/ACS_2007_11_Ferrett/Data_from_Extract/1/")

#Open Data from Data Ferrett
ACS_2007_11 <- read.table(file="DataFerrett_ACS20072011_CookCounty.asc", head=TRUE, sep=",")

#Separate margin of error columns from rest of the data frame
ACS_2007_11_geolabel <- ACS_2007_11[,1:9]
ACS_2007_11_vars <- ACS_2007_11[,10:ncol(ACS_2007_11)]
ACS_2007_11_est <- ACS_2007_11_vars[,seq(1,ncol(ACS_2007_11_vars),2)]
ACS_2007_11_margin_of_error <- ACS_2007_11_vars[,seq(2,ncol(ACS_2007_11_vars),2)]

#Rename Vars with Labels from Data Ferrett Codebook
varlist <- data.frame(read.csv(file="C:/Users/Zach/Documents/UrbanCCD/ACS_2007_11_Ferrett/Data_from_Extract/1/Ferrett_VarNames.txt", header=FALSE, blank.lines.skip=TRUE))
TableName <- t(data.frame(substring(varlist[c(seq(1,nrow(varlist),3)),],17)))
VarDesc <- t(data.frame(varlist[c(seq(3,nrow(varlist),3)),]))
Label <- t(data.frame(paste(TableName, VarDesc, sep = "-")))
names(ACS_2007_11_est) <- paste(Label, "Est", sep="-")
names(ACS_2007_11_margin_of_error) <- paste(Label, "Marg_of_Error", sep="-")

#Put Dataset Back Together and Send to CSV
ACS_2007_11 <- cbind(ACS_2007_11_geolabel, ACS_2007_11_est)
ACS_2007_11 <- cbind(ACS_2007_11, ACS_2007_11_margin_of_error)
rm(list=(ls()[ls()!="ACS_2007_11"]))
write.csv(ACS_2007_11, file="ACS_2007_11_Tract_BlockGroup.csv", row.names=FALSE)
