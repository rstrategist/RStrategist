# Import RAI daily data and convert to monthly
ImportData_RAI <- function()
{
  RAI <- read.table("ra_daily_update.csv",header=TRUE,sep=",",as.is = TRUE)                                 
  head(RAI)
  RAI <- RAI[,-3:-4] # Remove unwanted columns
  tail(RAI)

  # Convert d-m-y char to date
  tmpDate<-zoo::as.Date(RAI[,1],format="%d/%m/%Y")
  head(tmpDate)
  
  # Create XTS of with POSIXct index
  RAI <- xts(as.matrix(RAI[,-1]),as.POSIXct(paste(tmpDate),tz="Europe/London",format='%Y-%m-%d')) 
  #Name Columns
  colnames(RAI) <- "RAI"
  rm(list=ls(pattern="tmp"))
  head(RAI)
  plot(RAI,main="RAI")
  
  #Convert to monthly timeseries
  RAI <- to.monthly(RAI,indexAt='endof',OHLC = FALSE)
  head(RAI)
  # Data cleaning----
  
  # Remove rows with NA in OHLC data
  RAI <- na.omit(RAI)
  
  # Remove duplicated index rows
  RAI <- RAI[ ! duplicated( index(RAI), fromLast = TRUE ),  ]
  
  head(RAI,100)  
  tail(RAI,100)
  
  return(RAI)
}