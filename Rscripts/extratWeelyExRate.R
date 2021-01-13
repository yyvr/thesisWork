# weekly exchange rate, Friday's data
eurousd <- read.csv('/Users/vir/Downloads/Thesis/data/exchange_rate/eurusd.csv')
cnyusd <- read.csv('/Users/vir/Downloads/Thesis/data/exchange_rate/cnyusd.csv')
gbpusd <- read.csv('/Users/vir/Downloads/Thesis/data/exchange_rate/gbpusd.csv')

eurousd <- eurousd[order(nrow(eurousd):1), ]
cnyusd <- cnyusd[order(nrow(cnyusd):1), ]
gbpusd <- gbpusd[order(nrow(gbpusd):1), ]

dd <- seq(from = as.Date('2013-04-22'), 
          to = as.Date('2019-08-01'), 
          by = 'week')
weurusd <- NULL
wcnyusd <- NULL
wgbpusd <- NULL
counter <- 0
for (i in seq(5, nrow(eurousd), by=5))
{
  weurusd <- c(weurusd, eurousd$Price[i+4])
  wcnyusd <- c(wcnyusd, cnyusd$Price[i+4])
  wgbpusd <- c(wgbpusd, gbpusd$Price[i+4])
}
a <- cbind(as.character(dd[1:323]), weurusd[1:323], wcnyusd[1:323], wgbpusd[1:323])
write.csv(a, '/Users/vir/Downloads/Thesis/data/weekly_exchange_rates.csv')
