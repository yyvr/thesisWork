scmp <- read.csv('/Users/vir/Downloads/Thesis/data/monthly/monthlyscmpwithcontrol.csv')
ft <- read.csv('/Users/vir/Downloads/Thesis/data/monthly/monthlyftwithcontrol.csv')
pdata <- read.csv('/Users/vir/Downloads/Thesis/data/monthly/monthlyPanel.csv')

m1 <- lm(btc~pos + vix + gold + sse + ftse + pos:source, data = pdata)
m2 <- lm(btc~neg + vix + gold + sse + ftse + neg:source, data = pdata)
m3 <- lm(btc~pos + neg + vix + gold + sse + ftse + pos:source, data = pdata)
m4 <- lm(btc~pos + neg + vix + gold + sse + ftse + neg:source, data = pdata)
m5 <- lm(btc~lncount + vix + gold + sse + ftse + lncount:source, data = pdata)
m6 <- lm(btc~pos + neg + lncount + vix + gold + sse + ftse + lncount:source, data = pdata)

library('stargazer')
stargazer(m1, m2, m3, m4, m5, m6,
          type = 'html', out = '/Users/vir/Downloads/m.htm')
