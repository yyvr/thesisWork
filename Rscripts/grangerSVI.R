library('lmtest')
getGranger <- function(x, y, namex, namey, lags)
{
  pvalue <- NULL
  fstat <- NULL
  pvalue2 <- NULL
  fstat2 <- NULL
  for (i in lags)
  {
    # y dose not granger cause x
    a <- grangertest(x~y, order = i)
    if (a$`Pr(>F)`[2] <= 0.01)
      s <- '***'
    else if (a$`Pr(>F)`[2] <= 0.05)
      s <- '**'
    else if (a$`Pr(>F)`[2] <= 0.1)
      s <- '*'
    else s <- ''
    pvalue <- c(pvalue, paste(round(a$`F`[2], 2), s, sep=''))
    #fstat <- c(fstat, round(a$F[2], 2))
    b <- grangertest(y~x, order = i)
    if (b$`Pr(>F)`[2] <= 0.01)
      ss <- '***'
    else if (b$`Pr(>F)`[2] <= 0.05)
      ss <- '**'
    else if (b$`Pr(>F)`[2] <= 0.1)
      ss <- '*'
    else ss <- ''
    pvalue2 <- c(pvalue2, paste(round(b$`F`[2], 2), ss, sep=''))
    #fstat2 <- c(fstat2, round(b$F[2], 2))
  }
  tt <- rbind(pvalue, pvalue2)
  result <- cbind(c(paste(namey, 'does not Granger-cause',namex), 
                    paste(namex, 'does not Granger-cause',namey)), 
                  tt)
  rownames(result) <- NULL
  return (result)
}

btc <- read.csv('/Users/vir/Downloads/btc-usd-daily.csv')
svidaily <- read.csv2('/Users/vir/Downloads/Thesis/control_data/google_trends_daily_adjusted.csv',
                      sep = ';',
                      header = TRUE)
t1 <- getGranger(diff(btc$Adj.Close[23:2283]), diff(svidaily$adjusted_hists[22:2282]), 
                 'price', 'SVI', c(1, 2, 3, 7, 30, 100))
t12 <- getGranger(diff(log(btc$Adj.Close[23:2283])), diff(svidaily$adjusted_hists[22:2282]), 
                  'logPrice', 'SVI', c(1, 2, 3, 7, 30, 100))
t13 <- getGranger(diff(log10(btc$Adj.Close[23:2283])), diff(log10(svidaily$adjusted_hists[22:2282])), 
                  'logPrice', 'logSVI', c(1, 2, 3, 7, 30, 100))
t14 <- getGranger(diff(btc$Adj.Close[23:2283]), diff(log10(svidaily$adjusted_hists[22:2282])), 
                  'price', 'logSVI', c(1, 2, 3, 7, 30, 100))
xx <- rbind(t1, t12, t13, t14)

svi <- read.csv('/Users/vir/Downloads/Thesis/data/svi/google_trends_weekly.csv')
wbtc <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageBTC.csv')
svi <- svi[1:323, ]
t3 <- getGranger(diff(wbtc$weeklymean), diff(svi$wsvi), 'price', 'SVI', c(1, 2, 3, 4, 52, 78))
t32 <- getGranger(diff(log10(wbtc$weeklymean)), diff(svi$wsvi), 'logPrice', 'SVI', c(1, 2, 3, 4, 52, 78))
t33 <- getGranger(diff(log10(wbtc$weeklymean)), diff(log10(svi$wsvi)), 'logPrice', 'logSVI', c(1, 2, 3, 4, 52, 78))
t34 <- getGranger(diff(wbtc$weeklymean), diff(log10(svi$wsvi)), 'price', 'logSVI', c(1, 2, 3, 4, 52, 78))
yy <- rbind(t3, t32, t33, t34)

zz <- rbind(c('Null hypothesis', c(1, 2, 3, 7, 30, 100)), xx,
            c('Null hypothesis', c(1, 2, 3, 4, 52, 78)), yy)
library('stargazer')
stargazer(zz, type = 'html', out='/Users/vir/Downloads/granger.htm')
