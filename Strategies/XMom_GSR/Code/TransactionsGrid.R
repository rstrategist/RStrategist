# TransactionsGrid
# Calculates the transaction required for rebalancing the portfolio

# Test
# Strategy=a.strategy
# XTS <- IndicesXTS.EM[CurrentDate]
# BuySell=1
# NewInstrName <- LongShort.EM
# AllInstr <- symbols.EM


TransactionsGrid <- function(NewInstrName, Long.EM, Short.EM, AllInstr, CurrentDate, Strategy,XTS,BuySell)
{

# Check positions in new Sectors---
symbol <- NewInstrName[1]
tmpPosn <- list()
tmpPosnGrid <- list()
# Grid of all instruments and current positions in them
for(symbol in AllInstr){ 
  tmpPosnGrid[symbol] <- getPosQty(Strategy, Symbol=symbol, Date=CurrentDate)
}

# Grid of new NewInstrName to trade and current positions in them
for(symbol in NewInstrName){ 
  tmpPosn[symbol] <- getPosQty(Strategy, Symbol=symbol, Date=CurrentDate)
}


# Grid of new instruments to trade and volume to trade in them
# Copy PosnGrid and set new copied grid to zero
tmpPosnNewQty <- tmpPosnGrid
for(symbol in AllInstr){ 
  tmpPosnNewQty[symbol] <- 0
}

tmpPosnGrid[]
tmpPosn[]
tmpPosnNewQty[]

# Determine % equity to trade in each sector
# ***** INCLUDE UNREALISED P&L *****
equity = getEndEq(Strategy, CurrentDate)
for(symbol in Long.EM){
  sym = get(symbol)
  head(sym)
  (ClosePrice = as.numeric(sym[CurrentDate]))
  tmpPosnNewQty[symbol] <- BuySell*(1/3)*equity/ClosePrice # Adjust to required equal weighting in each pair  
}

for(symbol in Short.EM){
  sym = get(symbol)
  head(sym)
  (ClosePrice = as.numeric(sym[CurrentDate]))
  tmpPosnNewQty[symbol] <- -BuySell*(1/3)*equity/ClosePrice # Adjust to required equal weighting in each pair  
}


# Grid of trade quantity movement needed to arrive at new state
tmpPosnTrade <- list()
for(symbol in AllInstr){ 
  tmpPosnTrade[symbol] <- as.numeric(tmpPosnNewQty[symbol]) - as.numeric(tmpPosnGrid[symbol])
}

# Condensed grid of trades needed to ascertain new state
tmpPosnTrade <- tmpPosnTrade[tmpPosnTrade!=0]

return(tmpPosnTrade)

}