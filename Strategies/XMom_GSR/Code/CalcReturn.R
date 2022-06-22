#CalcReturn
CalcReturn <- function(XTS, CurrentDate, LookBackPeriod)
{
  # Calculate period log return
  head(XTS)
  ReturnXTS <- diff(XTS, lag = LookBackPeriod, differences = 1, arithmetic = FALSE, log = TRUE, na.pad = TRUE)
  
  # Remove rows with NA in OHLC data
  ReturnXTS <- na.omit(ReturnXTS)
  head(ReturnXTS)
  
  # Index return in decending order
  Return <- order(ReturnXTS[CurrentDate],decreasing=T)
  (ReturnXTS <- ReturnXTS[CurrentDate,Return])
  return(ReturnXTS)
}