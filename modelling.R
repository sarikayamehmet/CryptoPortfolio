#Read train data
df <- read.csv("trainData.csv")
df$T <- as.Date(df$T)
eth <- ts(df[df$MarketName=="BTC-ETH",c("O","C","H","L","V","BV","Mean","Reward")], start=c(2015,350), frequency=365.25) # Daily Data
ltc <- ts(df[df$MarketName=="BTC-LTC",c("O","C","H","L","V","BV","Mean","Reward")], start=c(2015,350), frequency=365.25) # Daily Data
xrp <- ts(df[df$MarketName=="BTC-XRP",c("O","C","H","L","V","BV","Mean","Reward")], start=c(2015,350), frequency=365.25) # Daily Data
xlm <- ts(df[df$MarketName=="BTC-XLM",c("O","C","H","L","V","BV","Mean","Reward")], start=c(2015,350), frequency=365.25) # Daily Data
usd <- ts(df[df$MarketName=="USDT-BTC",c("O","C","H","L","V","BV","Mean","Reward")], start=c(2015,350), frequency=365.25) # Daily Data

full.ts <- cbind(eth,ltc,xrp,xlm,usd)
full.df <- data.frame(DATE=as_date(date_decimal(as.numeric(time(full.ts)))), as.matrix(full.ts))
