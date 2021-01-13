btscmp <- read.csv2('/Users/vir/Downloads/Thesis/scmp/final/btc_sents_date.csv',
                    header=TRUE, 
                    sep=";")
btnews <- read.csv2('/Users/vir/Downloads/Thesis/ft/final/btc_news_sent_date.csv',
                    header=TRUE, 
                    sep=";")
btscmp$source <- 1
btnews$source <- 0
btscmp$X.1 <- NULL
btscmp$X <- NULL
btnews$X<- NULL
for (i in 1:nrow(btnews))
{
  btscmp <- rbind(btscmp, btnews[i, ])
}

countvolume <- function(btnews) {
  dd <- seq(from = as.Date(btnews$date[1]), 
            to = as.Date(btnews$date[nrow(btnews)]), 
            by = 'day')
  
  dailynewscounts <- NULL
  neg_score <- NULL
  pos_score <- NULL
  news_date <- NULL
  for (i in 1:length(dd))
  {
    indices <- which(as.Date(btnews$date) == dd[i])
    if (length(indices) != 0)
    {
      s <- mean(as.numeric(as.character(btnews$negative.[indices])))
      p <- mean(as.numeric(as.character(btnews$positive.[indices])))
      dailynewscounts <- c(dailynewscounts, length(indices))
      neg_score <- c(neg_score, s)
      pos_score <- c(pos_score, p)
      news_date <- c(news_date, as.character(dd[i]))
    }
  }
  ret <- cbind(news_date, neg_score, pos_score, dailynewscounts)
  return(ret)
}
ftcounts <- countvolume(btnews)
ftcounts <- cbind(ftcounts, 1)
ftdf <- data.frame(ftcounts)
colnames(ftdf)[5] <- c('source')

scmpcounts <- countvolume(btscmp)
scmpcounts <- cbind(scmpcounts, 0)
scmpdf <- data.frame(scmpcounts)
colnames(scmpdf)[5] <- c('source')
countall <- rbind(ftdf, scmpdf)
zz <- countall[order(countall$news_date), ]
write.csv(zz, '/Users/vir/Downloads/Thesis/data/panel_data_count_sent_source.csv')

all <- rbind(btscmp, btnews)
ffx <- all[order(all$date), ]
write.csv(ffx, '/Users/vir/Downloads/Thesis/data/scmp_ft_sent_date.csv')

total <- merge(btscmp,btnews,by = intersect(names(btscmp), names(btnews)))

btcPrices <- read.csv2("/Users/vir/Downloads/Thesis/ft/tmp/BitcoinPricesReformat.csv")
abc <- NULL
for (i in 2:nrow(zz))
{
  index <- which(as.Date(zz$news_date[i]) == as.Date(btcPrices$Date))
  if (length(index) != 0)
  {
    pp <- NULL
    for (j in 0:20)
    {
      pp <- c(pp, as.numeric(as.character(btcPrices$Close[index + j])))
    }
    pp <- c(as.character(zz$news_date[i]), 
            as.numeric(as.character(zz$neg_score[i])), 
            as.numeric(as.character(zz$pos_score[i])), 
            as.numeric(as.character(zz$dailynewscounts[i])), 
            as.numeric(as.character(zz$source[i])), pp)
  }
  if (length(index) > 1)
  {
    print(zz$news_date[i])
  }
  abc <- rbind(abc, pp)
}

ddd <- data.frame(abc)
colnames(ddd) <- c('date', 'neg_score', 'pos_score', 'count', 'source', 
                   'p0', 'p1', 'p2', 'p3', 'p4', 'p5', 'p6', 'p7', 'p8', 'p9', 'p10',
                   'p11', 'p12', 'p13', 'p14', 'p15', 'p16', 'p17', 'p18', 'p19', 
                   'p20')
write.csv(ddd, '/Users/vir/Downloads/Thesis/data/panel_data_count_sent_source_20prices.csv')
paneldata <- ddd
paneldata$rf <- NULL


