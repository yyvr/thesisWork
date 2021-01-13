scmp <- read.csv('/Users/vir/Downloads/Thesis/data/monthly/monthlyscmpwithcontrol.csv')
ft <- read.csv('/Users/vir/Downloads/Thesis/data/monthly/monthlyftwithcontrol.csv')
pdata <- read.csv('/Users/vir/Downloads/Thesis/data/monthly/monthlyPanel.csv')

m1 <- lm(btc~pos + vix + gold + ftse, data = ft)
m2 <- lm(btc~neg + vix + gold + ftse, data = ft)
m3 <- lm(btc~pos + neg + vix + gold + ftse, data = ft)
m4 <- lm(btc~lncount + vix + gold + ftse, data = ft)
m5 <- lm(btc~pos + neg + lncount + vix + gold + ftse, data = ft)

# Heteroskedasticity Breusch-Pagan test
library('lmtest')
lmtest::bgtest(m1)
# NCV test
car::ncvTest(m1)
library('sandwich')
stargazer(coeftest(m1, vcovHC), coeftest(m2, vcovHC), coeftest(m3, vcovHC),coeftest(m21, vcovHC),
          type = 'html', out = '/Users/vir/Downloads/corrected.htm')
library('car')
m11 <- lm(btc~pos + vix + gold + sse, data = scmp)
m12 <- lm(btc~neg + vix + gold + sse, data = scmp)
m13 <- lm(btc~pos + neg + vix + gold + sse, data = scmp)
m14 <- lm(btc~lncount + vix + gold + sse, data = scmp)
m15 <- lm(btc~pos + neg + lncount + vix + gold + sse, data = scmp)

m111 <- lm(btc~pos + vix + gold + sse + ftse + source, data = pdata)
m112 <- lm(btc~neg + vix + gold + sse + ftse + source, data = pdata)
m113 <- lm(btc~pos + neg + vix + gold + sse + ftse + source, data = pdata)
m114 <- lm(btc~lncount + vix + gold + sse + ftse + source, data = pdata)
m115 <- lm(btc~pos + neg + lncount + vix + gold + sse + ftse + source, data = pdata)
library('stargazer')
stargazer(m1, m2, m3, m4, m5, m11, m12, m13, m14, m15,
          type = 'html', out = '/Users/vir/Downloads/m.htm')

stargazer(m111, m112, m113, m114, m115,
          type = 'html', out = '/Users/vir/Downloads/m1.htm')

d <- cbind(ft$pos, ft$neg, scmp$pos, scmp$neg, ft$btc, ft$vix, ft$gold, ft$sse, ft$ftse)
colnames(d) <- c('FTpos', 'FTneg', 'SCMPpos', 'SCMPneg', 'lnReturn', 'lnVix', 'lnGold', 'lnSse', 'lnFtse')
library('stargazer')
stargazer(data.frame(d), type = 'html', out = '/Users/vir/Downloads/sum2.htm')

mcor <- as.matrix(round(cor(d, use = "pairwise.complete.obs"), 2))
stargazer(mcor, type = 'html', out='/Users/vir/Downloads/cor2.htm',digits=1)
