btnews <- read.csv2('/Users/vir/Downloads/Thesis/scmp/final/btc_sents_date.csv',
                    header=TRUE, 
                    sep=";")
btnews$X <- NULL
dd <- seq(from = as.Date('2013-04-22'), 
          to = as.Date('2019-08-01'), 
          by = 'week')

weeklycounts <- NULL
weeklycounts$week <- NA
weeklycounts$count <- NA
weeklycounts$neg <- NA
weeklycounts$pos <- NA
for (i in 1:length(dd))
{
  weeklycounts$week[i] <- as.character(dd[i])
  weekdata <- btnews[btnews$date >= dd[i] & btnews$date < dd[i+1], ]
  weeklycounts$count[i] <- nrow(weekdata)
  weeklycounts$neg[i] <- sum(as.numeric(weekdata$negative.))
  weeklycounts$pos[i] <- sum(as.numeric(weekdata$positive.))
}

write.csv(weeklycounts, '/Users/vir/Downloads/Thesis/scmp/final/btcnews_weeklycounts.csv')
mdd <- seq(from = as.Date('2013-04-01'), 
          to = as.Date('2019-08-01'), 
          by = 'month')

monthlycounts <- NULL
monthlycounts$month <- NA
monthlycounts$count <- NA
monthlycounts$neg <- NA
monthlycounts$pos <- NA
for (i in 1:length(mdd))
{
  monthlycounts$month[i] <- as.character(mdd[i])
  monthdata <- btnews[btnews$date >= mdd[i] & btnews$date < mdd[i+1], ]
  monthlycounts$count[i] <- nrow(monthdata)
  monthlycounts$neg[i] <- sum(as.numeric(monthdata$negative.))
  monthlycounts$pos[i] <- sum(as.numeric(monthdata$positive.))
}
write.csv(monthlycounts, '/Users/vir/Downloads/Thesis/ft/final/btcnews_monthlycounts.csv')


