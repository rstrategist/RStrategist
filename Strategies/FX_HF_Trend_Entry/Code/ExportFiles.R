#ExportFiles

#Export Files---------------------------------------------------------------

ExportFiles <- function(ts)
  
{
  write.zoo(ts, file = "ts.csv",sep=",",index.name = "Index")
  
  #Performance Analytics
  thePortfolio = getPortfolio('default')
  theAccount = getAccount('default')
  Transactions <- getTxns(Portfolio="default", Symbol=symbol)
  
  write.zoo(thePortfolio$summary, file = "Portfolio_Summary_Export.csv",sep=",",index.name = "Index")
  write.zoo(theAccount$summary, file = "Account_Summary_Export.csv",sep=",",index.name = "Index")
  write.zoo(Transactions, file = "Transactions_Summary_Export.csv",sep=",",index.name = "Index")
  write.zoo(EntrySignals, file = "EntrySignals_Export.csv",sep=",",index.name = "Index")
  write.zoo(ExitSignals, file = "ExitSignals_Export.csv",sep=",",index.name = "Index")  
}

