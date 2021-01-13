svi <- read.csv2('/Users/vir/Downloads/Thesis/control_data/google_trends_daily_adjusted.csv',
                 header=TRUE, 
                 sep=";")
wsvi <- read.csv('/Users/vir/Downloads/Thesis/control_data/google_trends_weekly.csv')

sundaysvi <- NULL
dd <- NULL
for (i in seq(28, 2286, 7))
{
  sundaysvi <- c(sundaysvi, svi$adjusted_hists[i])
  dd <- c(dd, svi$date[i])
}

write.csv(cbind(wsvi, sundaysvi, ))
write.csv(cbind(dd, sundaysvi), 
          '/Users/vir/Downloads/Thesis/control_data/google_trends_sunday.csv')
# load package ----
library(tsm)
library(vars)
library(mFilter)

getGranger <- function(x, y, namex, namey)
{
  pvalue <- NULL
  fstat <- NULL
  pvalue2 <- NULL
  fstat2 <- NULL
  for (i in 1:10)
  {
    # y dose not granger cause x
    a <- grangertest(x~y, order = i)
    pvalue <- c(pvalue, round(a$`Pr(>F)`[2], 2))
    fstat <- c(fstat, round(a$F[2], 2))
    b <- grangertest(y~x, order = i)
    pvalue2 <- c(pvalue2, round(b$`Pr(>F)`[2], 2))
    fstat2 <- c(fstat2, round(b$F[2], 2))
  }
  tt <- rbind(pvalue, fstat, pvalue2, fstat2)
  result <- cbind(c(paste(namey, 'does not Granger-cause',namex), '', 
                    paste(namex, 'does not Granger-cause',namey), ''), 
                  c('p-value', 'f-stat', 'p-value', 'f-stat'), 
                  tt)
  rownames(result) <- NULL
  return (result)
}
btc <- read.csv('/Users/vir/Downloads/btc-usd-daily.csv')

library(forecast)
# test for unit root and number of differences
# https://finance.yahoo.com/quote/BTC-USD/history?period1=1364763600&period2=1564520400&interval=1d&filter=history&frequency=1d
ndiffs(btc$Adj.Close, alpha = 0.05, test=c('kpss'))
ndiffs(svi$adjusted_hists, alpha = 0.05, test=c('kpss'))

r <- getGranger(diff(log10(btc$Adj.Close[2:2313])), 
                diff(svi$adjusted_hists[1:2312]), 
                'logPrice', 'SVI')
rwsvi <- getGranger((scale(diff(sundaysvi))),
                    scale(diff(log10(ft$price))),
                    'Weekly SVI', 'logPrice')

rswsvi <- getGranger(scale(diff(wsvi$wsvi[1:323])),
                    scale(diff(log10(ft$price))),
                    'Sum of weekly SVI', 'logPrice')
write.csv(cbind(diff(log10(btc$Adj.Close[2:2313])),
                diff(svi$adjusted_hists[1:2312])),
          '/Users/vir/Downloads/Thesis/results/granger/svipricedaily.csv')

a 
write.csv(cbind(diff(sundaysvi), diff(wsvi$wsvi[1:323]), diff(log10(ft$price)),
                diff(ft$count), diff(ft$abn_pos), diff(scmp$count), diff(scmp$abn_pos)),
          '/Users/vir/Downloads/Thesis/results/granger/weekly.csv')

ft <- read.csv('/Users/vir/Downloads/Thesis/data/adjusted_price_sent_stoxx_gold.csv')
ndiffs(ft$count, alpha = 0.05, test=c('kpss'))
ndiffs(log10(ft$price), alpha = 0.05, test=c('kpss'))
ndiffs(ft$abn_pos, alpha = 0.05, test=c('kpss'))
rftc <- getGranger(scale(diff(ft$count)),
                  scale(diff(log10(ft$price))), 
                  'FT news volume', 'logPrice')
rfts <- getGranger(scale(diff(ft$abn_pos)),
                  scale(diff(log10(ft$price))), 
                  'FT news sentiment', 'logPrice')

svift <- getGranger(scale(diff(ft$count)),
                    scale(diff(wsvi$wsvi[1:323])), 
                    'FT news volume', 'Sum of weekly SVI')
# scmp sentiment and price
scmp <- read.csv('/Users/vir/Downloads/Thesis/data/scmp_with_controlv.csv')
ndiffs(scmp$count, alpha = 0.05, test=c('kpss'))
ndiffs(scmp$price, alpha = 0.05, test=c('kpss'))
ndiffs(diff(scmp$abn_pos), alpha = 0.05, test=c('kpss'))

rscmpc <- getGranger(scale(diff(scmp$count)),
                   scale(diff(log10(scmp$price))), 
                   'SCMP news volume', 'logPrice')
rscmps <- getGranger(scale(diff(scmp$abn_pos)),
                   scale(diff(log10(scmp$price))), 
                   'SCMP news sentiment', 'logPrice')

sviscmp <- getGranger(scale(diff(scmp$count)),
                    scale(diff(wsvi$wsvi[1:323])), 
                    'SCMP news volume', 'Sum of weekly SVI')
fresult <- rbind(r, rswsvi, rwsvi, svift, sviscmp)
colnames(fresult) <- c('', '', 1:10)
stargazer(fresult,
          type = 'html', out='/Users/vir/Downloads/output.htm')

fresult <- rbind(rftc, rfts, rscmpc, rscmps)
colnames(fresult) <- c('', '', 1:10)
stargazer(fresult,
          type = 'html', out='/Users/vir/Downloads/output.htm')

attention <- cbind(scale(diff(wsvi$wsvi[1:323])), 
                   scale(diff(ft$count)))
colnames(attention) <- c('wsvi', 'ft count')
v <- VAR(attention, ic="AIC", lag.max=30, type="const")
causality(v, cause = 'wsvi')
causality(v, cause = 'ft.count')


