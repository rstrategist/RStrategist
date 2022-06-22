# ImportData_Indices
# Imports all available data for DM and EM indices

ImportData_Indices <- function()
{
  IndexXTS <- read.table("GSR.csv",header=TRUE,sep=",",as.is = TRUE)

  # Convert d-m-y char to date
  tmpDate<-zoo::as.Date(IndexXTS[,1],format="%d/%m/%Y")
  
  # Create XTS of FX with POSIXct index
  IndexXTS <- xts(as.matrix(IndexXTS[,-1]),as.POSIXct(paste(tmpDate),tz="GMT",format='%Y-%m-%d')) 
  
  rm(list=ls(pattern="tmp"))
  #head(IndexXTS)
  #plot(IndexXTS,main="Indices")
  
  
  # Data cleaning----
  
  # Remove rows with NA in OHLC data
  IndexXTS <- na.omit(IndexXTS)
  
  # Remove duplicated index rows
  IndexXTS <- IndexXTS[ ! duplicated( index(IndexXTS), fromLast = TRUE ),  ]
  
  head(IndexXTS,100)  
  tail(IndexXTS,100) 
  
  return(IndexXTS)
}



# Create_Indices <- function(Index,Frequency)
# {
#   # Convert TS to timescale----
#   Index.Daily <- IndexXTS
#   #colnames(Index.Daily) <- c('Close')
#   #colnames(Index.Carry.Daily) <- c('Close')
#   head(Index.Daily)
#   tail(Index.Daily)
#   
#   # Convert to 6M XTS
#   to.yearly(Index.Daily, drop.time=TRUE, name=NULL)
#   
#   Index.6Monthly=to.period(Index.Daily,
#                            period = 'months', 
#                            k = 6, 
#                            indexAt='endof', 
#                            name=NULL,
#                            OHLC = FALSE)
#   
#   Index.Monthly=to.monthly(Index.Daily, indexAt='endof')
#   
#   
#   colnames(Index.Monthly) <- c('Open','High','Low','Close')
#   colnames(Index.6Monthly) <- c('Open','High','Low','Close')
#   head(Index.Monthly)
#   head(Index.6Monthly)
#   plot(Index.Monthly,main=paste(Index,"Monthly",sep=" "))
#   plot(Index.6Monthly,main=paste(Index,"6Monthly",sep=" "))
#   
#   colnames(Index.Daily) <- c('Close')
#   colnames(Index.Monthly) <- c('Open','High','Low','Close')
#   colnames(Index.6Monthly) <- c('Open','High','Low','Close')
#   
#   # Data cleaning----
#   # If no close value in TS, replaces with prior close value 
#   for (i in 1:length(Index.Monthly[,4])){
#     if(is.na(Index.Monthly[i,4]) && i!=1) Index.Monthly[i,5] <- Index.Monthly[i-1,4]
#   }
#   
#   for (i in 1:length(Index.6Monthly[,4])){
#     if(is.na(Index.6Monthly[i,4]) && i!=1) Index.6Monthly[i,5] <- Index.6Monthly[i-1,4]
#   }
#   
#   # Remove rows with NA in OHLC data
#   Index.Monthly <- na.omit(Index.Monthly)
#   Index.6Monthly <- na.omit(Index.6Monthly)
#   #Index.Monthly[complete.cases(Index.Monthly[,1:4]),]
#   
#   # Remove duplicated index rows
#   Index.Daily <- Index.Daily[ ! duplicated( index(Index.Daily), fromLast = TRUE ),  ]
#   Index.Monthly <- Index.Monthly[ ! duplicated( index(Index.Monthly), fromLast = TRUE ),  ]  
#   Index.6Monthly <- Index.6Monthly[ ! duplicated( index(Index.6Monthly), fromLast = TRUE ),  ] 
#   
#   head(Index.Daily,100)
#   head(Index.Monthly,100)
#   head(Index.6Monthly,100)
#   
#   if(Frequency == "Daily") {return(Index.Daily)}
#   if(Frequency == "Monthly") {return(Index.Monthly)}
#   if(Frequency == "6M") {return(Index.6Monthly)}
# 
# }


