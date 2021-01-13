# BTC price data from yahoo finance
bitcoinPrices <- read.csv("/Users/vir/Downloads/Thesis/control_data/btcyahoo.csv")
pdata$volume <- NA
for (i in 1:nrow(pdata))
{
  index <- which(as.Date(pdata$date[i]) == as.Date(bitcoinPrices$Date))
  if (length(index) > 0) 
  {
    pdata$volume[i] <- as.numeric(as.character(bitcoinPrices$Volume[index]))
    for (j in 0:20)
    {
      pdata[i, j+6] <- as.numeric(as.character(bitcoinPrices$Adj.Close[index + j]))
    }
  }
}
plot(pdata$p0)
