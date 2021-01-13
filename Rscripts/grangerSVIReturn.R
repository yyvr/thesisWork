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

btc <- read.csv('/Users/vir/Downloads/Thesis/control_data/btcyahoo.csv')
svidaily <- read.csv2('/Users/vir/Downloads/Thesis/control_data/google_trends_daily_adjusted.csv',
                      sep = ';',
                      header = TRUE)
t1 <- getGranger(diff(log(btc$Close/btc$Open)[49:2309]), diff(svidaily$adjusted_hists[22:2282]), 
                 'lnReturn', 'SVI', c(1, 2, 3, 7, 30, 100))
t12 <- getGranger(diff(log(btc$Close/btc$Open)[49:2309]), diff(log(svidaily$adjusted_hists[22:2282])), 
                  'lnReturn', 'lnSVI', c(1, 2, 3, 7, 30, 100))
xx <- rbind(t1, t12)

newbtc <- btc[49:2309, ]
dd <- seq(from = as.Date('2013-04-22'), 
          to = as.Date('2019-07-01'), 
          by = 'week')
weeklyr <- NULL
for (i in 1:length(dd))
{
  indices <- which(as.Date(as.character(newbtc$Date)) >= dd[i] & 
                     as.Date(as.character(newbtc$Date)) < dd[i+1])
  if (length(indices) != 0)
  {
    x <- newbtc[indices, ]
    weeklyr <- c(weeklyr, log(x$Adj.Close[length(indices)]/x$Adj.Close[1]))
  }
}

svi <- read.csv('/Users/vir/Downloads/Thesis/data/svi/google_trends_weekly.csv')

svi <- svi[1:323, ]
t3 <- getGranger(diff(weeklyr), diff(svi$wsvi), 'lnReturn', 'SVI', c(1, 2, 3, 4, 52, 78))
t33 <- getGranger(diff(weeklyr), diff(log(svi$wsvi)), 'lnReturn', 'lnSVI', c(1, 2, 3, 4, 52, 78))
t4 <- getGranger(diff(ft$count), diff(weeklyr), 'newsVol', 'lnReturn', c(1, 2, 3, 4, 52, 78))
t5 <- getGranger(diff(ft$abn_pos), diff(weeklyr), 'abnPos', 'lnReturn', c(1, 2, 3, 4, 52, 78))
t6 <- getGranger(diff(scmp$count), diff(weeklyr), 'newsVol', 'lnReturn', c(1, 2, 3, 4, 52, 78))
t7 <- getGranger(diff(scmp$abn_pos), diff(weeklyr), 'abnPos', 'lnReturn', c(1, 2, 3, 4, 52, 78))

yy <- rbind(t3, t33, t4, t5, t6, t7)

zz <- rbind(c('Null hypothesis', c(1, 2, 3, 7, 30, 100)), xx,
            c('Null hypothesis', c(1, 2, 3, 4, 52, 78)), yy)
library('stargazer')
stargazer(zz, type = 'html', out='/Users/vir/Downloads/granger.htm')
