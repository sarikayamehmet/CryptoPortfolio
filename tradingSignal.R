

crypto.arb <- function (days = 365, back = 0, lookback = days, d.r = 20, d.v = 20, d.i = 20, ix.lower = NA, ix.upper = 2)
{
  read.prc <- function(file, header = F, make.numeric = T)
  {
    x <- read.delim(file, header = header)
    x <- as.matrix(x)
    if(make.numeric)
      mode(x) <- "numeric"
    return(x)
  }
  
  calc.mv.avg <- function(x, days, d.r)
  {
    if(d.r == 1)
      return(x[, 1:days])
    y <- matrix(0, nrow(x), days)
    for(i in 1:days)
      y[, i] <- rowMeans(x[, i:(i + d.r - 1)], na.rm = T)
    return(y)
  }
  
  prc <- read.prc("cr.prc.txt")
  cap <- read.prc("cr.cap.txt")
  high <- read.prc("cr.high.txt")
  low <- read.prc("cr.low.txt")
  vol <- read.prc("cr.vol.txt")
  open <- read.prc("cr.open.txt")
  mnbl <- read.prc("cr.mnbl.txt")
  name <- read.prc("cr.name.txt", make.numeric = F)
  
  d <- days + d.r + 1
  prc <- prc[, 1:d]
  cap <- cap[, 1:d]
  high <- high[, 1:d]
  low <- low[, 1:d]
  vol <- vol[, 1:d]
  open <- open[, 1:d]
  
  take <- rowSums(is.na(prc)) == 0 & rowSums(is.na(cap)) == 0 &
    rowSums(is.na(high)) == 0 & rowSums(is.na(low)) == 0 &
    rowSums(is.na(vol)) == 0 & rowSums(is.na(open)) == 0 &
    rowSums(vol == 0) == 0
  
  ret <- log(prc[take, -d] / prc[take, -1])
  ret.d <- prc[take, -d] / prc[take, -1] - 1
  prc <- prc[take, -1]
  cap <- cap[take, -1]
  high <- high[take, -1]
  low <- low[take, -1]
  vol <- vol[take, -1]
  open <- open[take, -1]
  mnbl <- mnbl[take, 1]
  name <- name[take, 1]
  
  if(back > 0)
  {
    ret <- ret[, (back + 1):ncol(ret)]
    ret.d <- ret[, (back + 1):ncol(ret)]
    prc <- prc[, (back + 1):ncol(prc)]
    cap <- cap[, (back + 1):ncol(cap)]
    high <- high[, (back + 1):ncol(high)]
    low <- low[, (back + 1):ncol(low)]
    vol <- vol[, (back + 1):ncol(vol)]
    open <- open[, (back + 1):ncol(open)]
  }
  days <- lookback
  
  av <- log(calc.mv.avg(vol, days, d.v))
  hlv <- (high - low)^2 / prc^2
  hlv <- 0.5 * log(calc.mv.avg(hlv, days, d.i))
  take <- rowSums(!is.finite(hlv)) == 0 ### removes stale prices
  
  av <- av[take, ]
  hlv <- hlv[take, ]
  mom <- log(prc[take, 1:days] / prc[take, 1:days + 1])
  size <- log(cap)[take, 1:days]
  ret <- ret[take, 1:days]
  ret.d <- ret.d[take, 1:days]
  mnbl <- mnbl[take]
  name <- name[take]
  
  pnl <- rep(0, days)
  
  for(i in days:1)
  {
    x <- -sign(mom[, i]) ### momentum based trading signal
    ### x[] <- 1 ### no signal
    ### x <- -x ### reverse signal
    
    sort.size <- sort(size[, i], decreasing = T)
    if(is.na(ix.lower))
      ix.lower <- length(sort.size)
    take <- size[, i] >= sort.size[ix.lower] &
      size[, i] <= sort.size[ix.upper] ### cap tier based universe
    
    x[!take] <- 0
    x[1] <- 0
    if(days > 365)
      x[name == "Circuits of V..." ] <- 0 ### removes COVAL
    
    x[x < 0] <- 0
    ### ss <- exp(hlv[, i]) ### hlv based volatility
    ### ss can be computed e.g. as 20-day historical return volatility
    ### x <- x / ss ### volatility-suppressed signal
    ### x <- x * abs(mom[, i]) / ss^2 ### alternative suppression
    x <- x / sum(abs(x))
    x <- x * ret.d[, i]
    x <- sum(x) - ret.d[1, i]
    if(!is.finite(x))
      x <- 0
    pnl[i] <- x
  }

  pnl1 <- pnl
  pnl <- pnl[days:1]
  pnl <- cumsum(pnl)
  plot(1:days, pnl, type = "l",
       col = "green", xlab = "days", ylab = "P&L")
  
  roc <- round(mean(pnl1) * 365 / 2 * 100, 2) ### annualized ROC
  sr <- round(mean(pnl1) / sd(pnl1) * sqrt(365), 2) ### annualized Sharpe
  print(roc)
  print(sr)
}

  
  