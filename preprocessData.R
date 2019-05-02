library(lubridate)
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

library(highcharter)
library(lubridate) # ymd
library(dplyr) # group_by

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

