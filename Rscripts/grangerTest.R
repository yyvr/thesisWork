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

scmp <- read.csv('/Users/vir/Downloads/Thesis/data/scmp_with_controlv.csv')

svi <- read.csv('/Users/vir/Downloads/Thesis/data/svi/google_trends_weekly.csv')
wbtc <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageBTC.csv')
svi <- svi[1:323, ]
t2 <- getGranger(diff(scmp$count), diff(svi$wsvi), 
                 'newsVol', 'SVI', c(1, 2, 3, 4, 52, 78))
t21 <- getGranger(diff(scmp$count), diff(log10(svi$wsvi)), 
                  'newsVol', 'logSVI', c(1, 2, 3, 4, 52, 78))

t3 <- getGranger(diff(scmp$count), diff(wbtc$weeklymean), 
                 'newsVol', 'price', c(1, 2, 3, 4, 52, 78))
t31 <- getGranger(diff(scmp$count), diff(log10(wbtc$weeklymean)), 
                  'newsVol', 'logPrice', c(1, 2, 3, 4, 52, 78))

t4 <- getGranger(diff(scmp$abn_pos), diff(wbtc$weeklymean), 
                 'abnPos', 'price', c(1, 2, 3, 4, 52, 78))
t41 <- getGranger(diff(scmp$abn_pos), diff(log10(wbtc$weeklymean)), 
                  'abnPos', 'logPrice', c(1, 2, 3, 4, 52, 78))

t5 <- getGranger(diff(scmp$abn_pos), diff(scmp$count), 
                 'abnPos', 'newsVol', c(1, 2, 3, 4, 52, 78))

xx <- rbind(c('Null hypothesis', c(1, 2, 3, 4, 52, 78)), 
            t2, t21, t3, t31, t4, t41, t5)
library('stargazer')
stargazer(xx, type = 'html', out='/Users/vir/Downloads/grangerscmp.htm')