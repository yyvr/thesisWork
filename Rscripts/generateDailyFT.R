ft <- read.csv2('/Users/vir/Downloads/Thesis/scmp/final/btc_sents_date.csv', sep = ';', header = TRUE)
ft$positive. <- as.numeric(as.character(ft$positive.))
ft$negative. <- as.numeric(as.character(ft$negative.))
dd <- seq(from = as.Date('2013-04-22'), 
          to = as.Date('2019-07-01'), 
          by = 'day')

dpos <- NULL
dneg <- NULL
dcount <- NULL
date <- NULL
for (i in 1:length(dd))
{
  index <- which(as.character(ft$date) == as.character(dd[i]))
  if (length(index) == 0) next
  chosen <- ft[index, ]
  pos <- mean(chosen$positive.)
  neg <- mean(chosen$negative.)
  dpos <- c(dpos, pos)
  dneg <- c(dneg, neg)
  dcount <- c(dcount, nrow(chosen))
  date <- c(date, as.character(dd[i]))
}
write.csv(cbind(date, dpos, dneg, dcount), '/Users/vir/Downloads/Thesis/scmp/final/daily.csv')
