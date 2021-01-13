ft <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyftBtcdata.csv')
scmp <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyscmpBtcdata.csv')
ft<- ft[1:323, ]
scmp <- scmp[1:323, ]
wbtc <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageBTC.csv')
wvix <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageVix.csv')
wgold <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageGold.csv')
wssa <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageSsa.csv')
wftse <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageFtse.csv')
ctd <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklylnReturnAllControldata.csv')

s <- read.csv('/Users/vir/Downloads/Thesis/data/monthly/monthlyscmpwithcontrol.csv')
f <- read.csv('/Users/vir/Downloads/Thesis/data/monthly/monthlyftwithcontrol.csv')

d <- cbind(wbtc$weeklymean, wvix$weeklymean, wgold$weeklymean, wssa$weeklymean, wftse$weeklymean,
           ctd$btc, ctd$vix, ctd$gold, ctd$sse, ctd$ftse,
           ft$sent, ft$count, scmp$sent, scmp$count, scale(ft$trend))
colnames(d) <- c('BTC price', 'vix', 'gold', 'sse', 'ftse', 
                 'lnReturn', 'lnVix', 'lnGold', 'lnSse', 'lnFtse',
                 '(FT) abnPos', '(FT) newsVol', '(SCMP) abnPos', '(SCMP) newsVol', 'svi')

library('stargazer')
stargazer(data.frame(d), type = 'html', out = '/Users/vir/Downloads/sum.htm')


