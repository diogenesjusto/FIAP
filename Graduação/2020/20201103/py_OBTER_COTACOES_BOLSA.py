import yfinance as yf
import pandas as pd

# Load main indexes used
idx = ["BRL=X",      # USD / R$
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
           "ABEV3.SA"]
idx = ' '.join(idx)

# Prepare
strTickers = acoes + idx

strTickers2 = strTickers[:9]

# Get the data
data = yf.download(tickers=strTickers, period="30d", interval="15m")