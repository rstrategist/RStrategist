# ExportFiles
# Functions to export data and reports

#Test
#  Strategy <- a.strategy
#  Directory <- OutputDirectory
#  AllInstr <- symbols.EM

#Export Files---------------------------------------------------------------

ExportFiles <- function(Strategy, Directory, AllInstr, XTS)
  
{
  setwd(Directory)
  getwd()
  # Performance Analytics----
  ThePortfolio = getPortfolio(Strategy)
  TheAccount = getAccount(Strategy)
  Transactions <- getTxns(Portfolio = Strategy, Symbol = symbol)
  
  for (a in AllInstr){
    print(a)
    print(getTxns(Portfolio = Strategy, Symbol = a))
  }
  
  write.zoo(ThePortfolio$summary, file = "GSR - Portfolio_Summary_Export.csv",sep=" ",index.name = "Index")
  write.zoo(TheAccount$summary, file = "GSR - Account_Summary_Export.csv",sep=" ",index.name = "Index")
  
  for (Instr in AllInstr){
    # Export transaction and PnL data
    write.zoo(ThePortfolio$symbols[[Instr]]$txn, file = paste("Transactions_",Instr,".csv",sep=""),index.name = "Index")
    write.zoo(ThePortfolio$symbols[[Instr]]$posPL, file = paste("Position_PnL_",Instr,".csv",sep=""),index.name = "Index")
    write.zoo(ThePortfolio$symbols[[Instr]]$posPL.USD, file = paste("Position_Pnl_DOLLAR_",Instr,".csv",sep=""),index.name = "Index")
  }
  
  # Export XTS
  write.zoo(XTS, file = "XTS.csv",index.name = "Index")
  
  # The Account-------
  #TheAccount = getAccount(a.strategy)
  #names(TheAccount)
  #names(TheAccount$portfolios)
  #names(TheAccount$portfolios[a.strategy])
  #names(TheAccount$summary)
  
  
  #write.zoo(EntrySignals, file = "EntrySignals_Export.csv",sep=",",index.name = "Index")
  #write.zoo(ExitSignals, file = "ExitSignals_Export.csv",sep=",",index.name = "Index")  
}


# Transactions <- getTxns(Portfolio = Strategy, Symbol = symbol)

