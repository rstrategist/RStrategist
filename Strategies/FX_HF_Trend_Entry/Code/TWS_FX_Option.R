# TWS FX Option

# twsOption(local,
#           expiry="",
#           strike="",
#           right="",
#           exch="SMART",
#           primary="",
#           currency='USD',
#           symbol='',
#           multiplier="100",
#           include_expired='0',
#           conId=0)

#Connect to TWS====
tws <- twsConnect()
reqCurrentTime(tws)

twsOption('P OCPU JUL 16 1450')

twsOption(local,
          expiry="", #option expiration CCYYMM [optional]
          strike="", #the strike price [optional]
          right="", #the requested right - 'C','CALL', 'P', or 'PUT' [optional]
          exch="SMART",
          primary="", #the primary exchange of the security [optional]
          currency='USD',
          symbol='',
          multiplier="100",
          include_expired='0',
          conId=0)