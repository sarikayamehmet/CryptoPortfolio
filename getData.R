library(rvest)
library(jsonlite)

datatype <- c("oneMin","fiveMin", "thirtyMin", "hour","day")
temp_url <- paste0("https://international.bittrex.com/Api/v2.0/pub/market/GetTicks?_=1499127220008&marketName=BTC-XRP&tickInterval=",datatype[5])
webpage <- fromJSON(temp_url)
df <- webpage$result
df$T <- as.Date(df$T)

library(highcharter)
highchart() %>%
  hc_xAxis(categories = df$T) %>%
  hc_add_series(name = "Open", data = df$O) %>%
  hc_add_series(name = "Closed", data = df$C) %>%
  hc_add_series(name = "High", data = df$H) %>%
  hc_add_series(name = "Low", data = df$L) %>%
  hc_colors(c("blue","red","green","yellow")) %>%
  hc_title(text="Yillara gore degisim",align="center") %>%
  hc_subtitle(text="D-Scrum",align="center") %>%
  hc_add_theme(hc_theme_elementary())

#Generic collector
getMarketData <- function(marketName, tickInterval="day"){
  temp_url <- paste0("https://international.bittrex.com/Api/v2.0/pub/market/GetTicks?_=1499127220008&marketName=",marketName,"&tickInterval=",tickInterval)
  webpage <- fromJSON(temp_url)
  df <- webpage$result
  df$MarketName <- marketName
  return(df)
}

#Collect All markets
url <- "https://bittrex.com/api/v1.1/public/getmarketsummaries"
webpage2 <- fromJSON(url)
df2 <- webpage2$result
marketNames <- df2$MarketName
BTCMarkets <- marketNames[grepl("BTC-", marketNames)]
tickList <- c("oneMin","fiveMin", "thirtyMin", "hour","day")
marketPrices <- getMarketData(BTCMarkets[1], tickList[5])
for (i in 25:length(BTCMarkets)) {
  tempDF <- getMarketData(BTCMarkets[i], tickList[5])
  marketPrices <- rbind(marketPrices, tempDF)
  Sys.sleep(3)
}


