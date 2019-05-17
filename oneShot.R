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
marketPrices <- getMarketData(top50BTCMarkets[1], tickList[5])
for (i in 2:length(top50BTCMarkets)) {
  tempDF <- getMarketData(top50BTCMarkets[i], tickList[5])
  marketPrices <- rbind(marketPrices, tempDF)
  tempDF <- NULL
}

#Save Data
marketPrices$T <- as.Date(marketPrices$T)
marketPrices$MarketName <- as.factor(marketPrices$MarketName)

#Preprocess
df <- marketPrices
#Calculate the period-over-period percentage change at each point in time
percentageCalc <- function(marketName){
  marketData <- df[df$MarketName==marketName,]
  #Open, Closed, High, Low, Volume, BaseVolume
  data <- marketData[,c("O","C","H","L","V","BV")]
  data$Mean <- (data$H + data$L)/2
  newdf <- (tail(data, -1) - head(data, -1))/data[-nrow(data),]
  newdf$T <- tail(marketData$T, -1)
  newdf$MarketName <- marketName
  newdf$Reward <- newdf$Mean*100
  return(newdf)
}

list_of_dataframes <- lapply(unique(df$MarketName), function(x) percentageCalc(x))
prepData <- bind_rows(list_of_dataframes, .id = "ID")

#Simplify data to quick test
# We will only use ETH, LTC, XRP, XLM, USDT for BTC market as Multi-armed bandit approach
baseMarkets <- c("BTC-ETH", "BTC-LTC", "BTC-XRP", "BTC-XLM", "USDT-BTC")
baseData <- prepData[prepData$MarketName %in% baseMarkets,]
# ETH -> "2015-08-15"
# LTC -> "2014-03-10"
# XRP -> "2014-12-23"
# XLM -> "2015-11-19"
# USDT -> "2015-12-15"
# So we take "2015-12-15" as min date to arbitrage test
baseData <- baseData[baseData$T >= "2015-12-15",]
#Fill missing
USDData <- baseData[baseData$MarketName=="USDT-BTC",] %>%
  mutate(T = as.Date(T)) %>%
  complete(T = seq.Date(min(T), max(T), by="day")) %>%
  #group_by(ID, MarketName) %>%
  fill(ID,MarketName,O,C,H,L,V,BV,Mean,Reward)

XLMData <- baseData[baseData$MarketName=="BTC-XLM",] %>%
  mutate(T = as.Date(T)) %>%
  complete(T = seq.Date(min(T), max(T), by="day")) %>%
  #group_by(ID, MarketName) %>%
  fill(ID,MarketName,O,C,H,L,V,BV,Mean,Reward)

#Merge all in a bundle
trainData <- baseData[baseData$MarketName!="USDT-BTC",]
trainData <- trainData[trainData$MarketName!="BTC-XLM",]
trainData <- rbind(trainData, XLMData, USDData)
#1235 day for each asset
write.csv(trainData, file = "trainData.csv", row.names = F)
