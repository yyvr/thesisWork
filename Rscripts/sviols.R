data <- read.csv('/Users/vir/Downloads/Thesis/data/svi/allweeklyscaled.csv')

wbtc <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageBTC.csv')
wvix <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageVix.csv')
wgold <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageGold.csv')
wssa <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageSsa.csv')
wftse <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageFtse.csv')

rdata <- data.frame(cbind(data$cnsvi, data$gbsvi, data$globsvi, log(wbtc$weeklymean), scale(wvix$weeklymean), 
                          scale(wgold$weeklymean), scale(wftse$weeklymean), scale(wssa$weeklymean)))

dcn <-  data.frame(cbind(data$cnsvi, log(wbtc$weeklymean), scale(wvix$weeklymean), scale(wgold$weeklymean), 
                         scale(wftse$weeklymean), scale(wssa$weeklymean), 'cn'))
dgb <- data.frame(cbind(data$gbsvi, log(wbtc$weeklymean), scale(wvix$weeklymean), scale(wgold$weeklymean),
                        scale(wftse$weeklymean), scale(wssa$weeklymean), 'gb'))

colnames(dcn) <- c('svi', 'lnPriceWeekly', 'vix', 'gold', 'ftse', 'sse', 'country')
colnames(dgb) <- c('svi', 'lnPriceWeekly', 'vix', 'gold', 'ftse', 'sse', 'country')

convertNum <- function(alld)
{
  alld$svi <- as.numeric(as.character(alld$svi))
  alld$lnPriceWeekly <- as.numeric(as.character(alld$lnPriceWeekly))
  alld$vix <- as.numeric(as.character(alld$vix))
  alld$gold <- as.numeric(as.character(alld$gold))
  alld$ftse <- as.numeric(as.character(alld$ftse))
  alld$sse <- as.numeric(as.character(alld$sse))
  return (alld)
}
alld <- rbind(dcn, dgb)
nalld <- convertNum(alld)
dcn <- convertNum(dcn)
dgb <- convertNum(dgb)

m1 <- lm(lnPriceWeekly~svi + gold + sse + vix , data = dcn)
m2 <- lm(lnPriceWeekly~svi + gold + ftse + vix , data = dgb)
#sviglob <- lm(logPrice~svi + gold + ftse + sse + vix , data = dgg)
m3 <- lm(lnPriceWeekly~svi + gold + ftse + sse + vix + country, data = nalld)
m4 <- lm(lnPriceWeekly~svi*country + gold + ftse + sse + vix, data = nalld)

dcn$lnPriceWeekly <- c(dcn$lnPriceWeekly[2:323], 9.322273)
dgb$lnPriceWeekly <- c(dgb$lnPriceWeekly[2:323], 9.322273)
nalld$lnPriceWeekly[1:323] <- c(dcn$lnPriceWeekly[2:323], 9.322273)
nalld$lnPriceWeekly[324:646] <- c(dcn$lnPriceWeekly[2:323], 9.322273)
m5 <- lm(lnPriceWeekly~svi + gold + sse + vix , data = dcn)
m6 <- lm(lnPriceWeekly~svi + gold + ftse + vix , data = dgb)
svifit <- lm(lnPriceWeekly~svi + gold + ftse + sse + vix + country, data = nalld)
sviinter2 <- lm(lnPriceWeekly~svi*country + gold + ftse + sse + vix, data = nalld)

summary(svifit)
library('stargazer')
stargazer(m1, m2, m3, m4, m5, m6, svifit, sviinter2,
          type = 'html', out = '/Users/vir/Downloads/svi.htm')
round(vif(m1),2)
round(vif(m2),2)
round(vif(m3),2)
round(vif(m4),2)
round(vif(m5),2)
round(vif(m6),2)
round(vif(svifit),2)
round(vif(sviinter2),2)
coeftest(m1, vcovHC)
