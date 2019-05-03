library(highcharter)
library(lubridate) # ymd
library(dplyr) # group_by
library(tidyr) #complete

#Read market data
df <- read.csv("marketPrices.csv")
df$T <- as.Date(df$T)
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

prepData$T <- datetime_to_timestamp(ymd(prepData$T)) # its easy to convert to a date
series_asset <- prepData %>%
  arrange(T) %>% 
  group_by(name = MarketName) %>% # we create a series for every category
  do(data = list.parse2(data_frame(.$T, .$Reward))) %>% # a line chart need IN ORDER the x and y
  list.parse3() # then convert every row in a list preserving names

highchart() %>% 
  hc_chart(type = "line") %>% 
  hc_xAxis(type = 'datetime',labels = list(format = '{Reward:%Y %m %d}')) %>% 
  hc_add_series_list(series_asset) %>%
  hc_title(text="Yillara gore tahmin",align="center") %>%
  hc_subtitle(text="2014-2019 icin degerler",align="center") %>%
  hc_add_theme(hc_theme_elementary())

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
