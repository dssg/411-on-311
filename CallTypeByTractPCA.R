#PCA CallTypeByTract!

##Read in full 311
full311ByTractCallType <- read.csv("~/Documents/DSSG/dssg-Indices-project/full311ByTractCallType.csv")

##Cast into a matrix of counts by CallType and Tract
callTypeByTract<-cast(full311ByTractCallType,geoid ~ service_description )

#Replace NAs with 0s Because and NA actually represents a 0
callTypeByTract[is.na(callTypeByTract)] <- 0 

#CULLEDCOLUMNS
    ##Make a Boolean Vector that represents which columns have a sum greater than MinNumberOfCallsOfTypeX
    MinNumberOfCallsOfTypeX<-10000
    TrueVector<-colSums(callTypeByTract, na.rm=TRUE)>MinNumberOfCallsOfTypeX

    ##Concatenate with a Leading TRUE to includ "geoid" and account for OBO error
    c(TRUE,TrueVector)

    #Create a new table with only those columns with a sufficient number of entries
    CallTypeByTractCulledColumns<-callTypeByTract[,TrueVector]

    #Calculate and print out the first Two Principal Components
    CallTypeByTractCulledColumns.pca <- prcomp(CallTypeByTractCulledColumns[,2:ncol(CallTypeByTractCulledColumns)], scale.=TRUE)
    round(CallTypeByTractCulledColumns.pca$rotation[,1:2],2)

    #percent of the total Variation described by the first PC
    (CallTypeByTractCulledColumns.pca$sdev[1])^2/(sum((CallTypeByTractCulledColumns.pca$sdev)^2))

    #percent of the total Variation described by the second PC
    (CallTypeByTractCulledColumns.pca$sdev[2])^2/(sum((CallTypeByTractCulledColumns.pca$sdev)^2))

#CULLEDROWS

    ##Make a Boolean Vector that represents which rows have a sum greater than MinNumberOfCallsInTract
    MinNumberOfCallsInTract<-1000
    TrueVector<-rowSums(callTypeByTract[,2:ncol(callTypeByTract]), na.rm=TRUE)>MinNumberOfCallsInTract

    #Create a new table with only those rows with a sufficient number of entries
    CallTypeByTractCulledRows<-callTypeByTract[TrueVector,]

    #Calculate and print out the first Two Principal Components
    CallTypeByTractCulledRows.pca <- prcomp(CallTypeByTractCulledRows[,2:ncol(CallTypeByTractCulledRows)], scale.=TRUE)
    round(CallTypeByTractCulledRows.pca$rotation[,1:2],2)

    #percent of the total Variation described by the first PC
    (CallTypeByTractCulledRows.pca$sdev[1])^2/(sum((CallTypeByTractCulledRows.pca$sdev)^2))

    #percent of the total Variation described by the second PC
    (CallTypeByTractCulledRows.pca$sdev[2])^2/(sum((CallTypeByTractCulledRows.pca$sdev)^2))

#CULLEDROWSANDCOLUMNS

    ##Make a Boolean Vector that represents which rows have a sum greater than MinNumberOfCallsInTract
    MinNumberOfCallsInTract<-1000
    TrueVector<-rowSums(CallTypeByTractCulledColumns[,2:ncol(CallTypeByTractCulledColumns]), na.rm=TRUE)>MinNumberOfCallsInTract

    #Create a new table with only those rows and columns with a sufficient number of entries
    CallTypeByTractCulledRowsAndColumns<-CallTypeByTractCulledColumns[TrueVector,]

    #Calculate and print out the first Two Principal Components
    CallTypeByTractCulledRowsAndColumns.pca <- prcomp(CallTypeByTractCulledRowsAndColumns[,2:ncol(CallTypeByTractCulledRowsAndColumns)], scale.=TRUE)
    round(CallTypeByTractCulledRowsAndColumns.pca$rotation[,1:2],2)

    #percent of the total Variation described by the first PC
    (CallTypeByTractCulledRowsAndColumns.pca$sdev[1])^2/(sum((CallTypeByTractCulledRowsAndColumns.pca$sdev)^2))

    #percent of the total Variation described by the second PC
    (CallTypeByTractCulledRowsAndColumns.pca$sdev[2])^2/(sum((CallTypeByTractCulledRowsAndColumns.pca$sdev)^2))


