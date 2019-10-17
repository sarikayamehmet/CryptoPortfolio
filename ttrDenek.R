library(TTR)
library(rvest)
library(jsonlite)
library(lubridate) # ymd
library(dplyr) # group_by
library(tidyr) #complete

#Generic collector
getMarketData <- function(marketName, tickInterval="day"){
  temp_url <- paste0("https://international.bittrex.com/Api/v2.0/pub/market/GetTicks?_=1499127220008&marketName=",marketName,"&tickInterval=",tickInterval)
  webpage <- fromJSON(temp_url)
  df <- webpage$result
  df$MarketName <- marketName
  return(df)
}

top50BTCMarkets <- c("BTC-ETH","BTC-XRP","BTC-LTC","BTC-XLM","USDT-BTC")
tickList <- c("oneMin","fiveMin", "thirtyMin", "hour","day")
marketPrices <- getMarketData("USDT-BTC", "day")
#Save Data
marketPrices$T <- as.Date(marketPrices$T)
marketPrices$MarketName <- as.factor(marketPrices$MarketName)
marketPrices$rsi <- RSI(marketPrices$C)
marketPrices$rsiMA1 <- RSI(marketPrices$C, n=14, maType="WMA", wts=marketPrices$V)
# Bollinger Bands
marketPrices$bbands <- BBands(marketPrices[,c("H","L","C")])
# Directional Movement Index
marketPrices$adx <- ADX(marketPrices[,c("H","L","C")])
# Moving Averages
marketPrices$ema <- EMA(marketPrices[,"C"], n=20)
marketPrices$sma <- SMA(marketPrices[,"C"], n=20)
# MACD
marketPrices$macd <- MACD( marketPrices[,"C"] )
# Stochastics
marketPrices$stochOsc <- stoch(marketPrices[,c("H","L","C")])


