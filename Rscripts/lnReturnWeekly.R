ft1 <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyFTNegPos.csv')
scmp1 <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklySCMPNegPos.csv')

ft <- read.csv('/Users/vir/Downloads/Thesis/results/ftdatascaled.csv')
scmp <- read.csv('/Users/vir/Downloads/Thesis/results/scmpdatascaled.csv')
cdata <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklylnReturnAllControldata.csv')
ftd <- cbind(ft$newsVol, ft1$neg, ft1$pos, cdata)
colnames(ftd) <- c('newsVol', 'neg', 'pos', 'lnReturn', 'lnVix', 'lnGold', 'lnSse', 'lnFtse')

scmpd <- cbind(scmp$newsVol, scmp1$negs, scmp1$poss,cdata)
colnames(scmpd) <- c('newsVol', 'neg', 'pos', 'lnReturn', 'lnVix', 'lnGold', 'lnSse', 'lnFtse')

ftd$source <- 'FT'
scmpd$source <- 'SCMP'
pd <- rbind(ftd, scmpd)
pd$source <- as.factor(pd$source)
m1 <- lm(lnReturn~pos + lnVix + lnGold + lnFtse, data = ftd)
m2 <- lm(lnReturn~neg + lnVix + lnGold + lnFtse, data = ftd)
m3 <- lm(lnReturn~pos + neg + lnVix + lnGold + lnFtse, data = ftd)
m4 <- lm(lnReturn~newsVol + lnVix + lnGold + lnFtse, data = ftd)
m5 <- lm(lnReturn~pos + neg + newsVol + lnVix + lnGold + lnFtse, data = ftd)

m11 <- lm(lnReturn~pos + lnVix + lnGold + lnSse, data = scmpd)
m12 <- lm(lnReturn~neg + lnVix + lnGold + lnSse, data = scmpd)
m13 <- lm(lnReturn~pos + neg + lnVix + lnGold + lnSse, data = scmpd)
m14 <- lm(lnReturn~newsVol + lnVix + lnGold + lnSse, data = scmpd)
m15 <- lm(lnReturn~pos + neg + newsVol + lnVix + lnGold + lnSse, data = scmpd)

library('stargazer')
stargazer(m1, m2, m3, m4, m5, m11, m12, m13, m14, m15,
          type = 'html', out = '/Users/vir/Downloads/m.htm')

m111 <- lm(lnReturn~pos + lnVix + lnGold + lnFtse + lnSse + pos:source, data = pd)
m112 <- lm(lnReturn~neg + lnVix + lnGold + lnFtse + lnSse + neg:source, data = pd)
m113 <- lm(lnReturn~pos + neg + lnVix + lnGold + lnFtse + lnSse + pos:source, data = pd)
m114 <- lm(lnReturn~pos + neg + lnVix + lnGold + lnFtse + lnSse + neg:source, data = pd)
m115 <- lm(lnReturn~newsVol + lnVix + lnGold + lnFtse + lnSse + newsVol:source, data = pd)
m116 <- lm(lnReturn~pos + neg + newsVol + lnVix + lnGold + lnFtse + lnSse + newsVol:source, data = pd)
stargazer(m111, m112, m113, m114, m115, m116,
          type = 'html', out = '/Users/vir/Downloads/m1.htm')