library(BatchGetSymbols)
library(dplyr)

acoes <- c(acoes, 
           "BRL=X",      # USD / R$
           "gold",       # ouro á vista
           "GCN20.CMX",  # futuro ouro 2MESES
           "CL=F",       # futuro de petróleo
           "GC=F",       # futuro de ouro
           "YM=F",       # futuro de dow jones
           "^N225",     # indice nikkei
           "^IXIC",     # indice nasdaq   
           "^GSPC",     # indice SP500
           "^DJI",      # dow jones industrial
           "^VIX",       # CBOE Volatility Index (
           "^TNX",       #Treasury Yield 10 Years ()
           "^RUT",       #Russell 2000 ()
           "^FTSE",      #FTSE Index - FTSE Index Delayed Price. Currency in GBP
           "^BVSP",      #Indice IBOVESPA
           "ETHUSD=X",   # Ethereum
           "BTCUSD=X",
		   "ABEV3.SA")   # Bitcoin

# Source start from Jan2020
inicio <- as.Date("2020-01-01")  # início
final <- Sys.Date()
bench.ticker <- "^BVSP"

###### 3. Getting data - source: yahoo finance
db <- BatchGetSymbols(tickers = acoes, first.date = inicio, last.date = final, 
                      bench.ticker = bench.ticker)

# Format dataframe from BatchGetSymbols objects structure
df<- db$df.tickers
df$ref.date <- as.character(df$ref.date)

# Persist data on disk
write.csv(df, paste(final,"-60d.csv",sep="") )
