library('Quandl')
gold <- Quandl("WGC/GOLD_DAILY_USD", api_key="HihTsXg4djxjDC9npch7",
               type='raw', order = c('asc'),
               start_date='2013-04-01',
               end_date='2019-07-10')
dd <- seq(from = as.Date('2013-04-22'), 
          to = as.Date('2019-07-01'), 
          by = 'month')

weeklym <- NULL
weeklyr <- NULL
for (i in 1:length(dd))
{
  indices <- which(as.Date(as.character(gold$Date)) >= dd[i] & 
                     as.Date(as.character(gold$Date)) < dd[i+1])
  if (length(indices) != 0)
  {
    x <- gold[indices, ]
    weeklym <- c(weeklym, mean(x$Value))
    weeklyr <- c(weeklyr, log(x$Value[length(indices)]/x$Value[1]))
  }
}

data <- cbind(as.character(dd), weeklym, weeklyr)
colnames(data) <- c('Date', 'weeklymean', 'weeklyr')
write.csv(data, '/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageGold.csv', row.names = FALSE)
