ft <- read.csv('/Users/vir/Downloads/Thesis/results/ftdatascaled.csv')
scmp <- read.csv('/Users/vir/Downloads/Thesis/results/scmpdatascaled.csv')
cdata <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklylnReturnAllControldata.csv')
#cdata[, c(2:5)] <- scale(cdata[, c(2:5)])
ftd <- cbind(ft$newsVol, ft$abnPos, cdata)
colnames(ftd) <- c('newsVol', 'abnPos', 'lnReturn', 'lnVix', 'lnGold', 'lnSse', 'lnFtse')

scmpd <- cbind(scmp$newsVol, scmp$abnPos, cdata)
colnames(scmpd) <- c('newsVol', 'abnPos', 'lnReturn', 'lnVix', 'lnGold', 'lnSse', 'lnFtse')

ftd$source <- 'FT'
scmpd$source <- 'SCMP'
pd <- rbind(ftd, scmpd)
pd$source <- as.factor(pd$source)
m1 <- lm(lnReturn~abnPos + lnVix + lnGold + lnFtse, data = ftd)
m2 <- lm(lnReturn~newsVol + lnVix + lnGold + lnFtse, data = ftd)
m3 <- lm(lnReturn~abnPos + newsVol + lnVix + lnGold + lnFtse, data = ftd)

m11 <- lm(lnReturn~abnPos + lnVix + lnGold + lnSse, data = scmpd)
m12 <- lm(lnReturn~newsVol + lnVix + lnGold + lnSse, data = scmpd)
m13 <- lm(lnReturn~abnPos + newsVol + lnVix + lnGold + lnSse, data = scmpd)

m111 <- lm(lnReturn~abnPos + lnVix + lnGold + lnFtse + lnSse + source, data = pd)
m112 <- lm(lnReturn~newsVol + lnVix + lnGold + lnFtse + lnSse + source, data = pd)
m113 <- lm(lnReturn~abnPos + newsVol + lnVix + lnGold + lnFtse + lnSse + source, data = pd)

library('stargazer')
stargazer(m1, m2, m3, m11, m12, m13,m111, m112, m113,
          type = 'html', out = '/Users/vir/Downloads/r.htm')
