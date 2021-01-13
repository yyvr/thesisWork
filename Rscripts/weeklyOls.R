wbtc <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageBTC.csv')
wvix <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageVix.csv')
wgold <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageGold.csv')
wssa <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageSsa.csv')
wftse <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageFtse.csv')

x <- cbind(as.character(wbtc$Date), wbtc$weeklymean, wvix$weeklymean, wgold$weeklymean, wssa$weeklymean, wftse$weeklymean)
colnames(x) <- c('Date', 'btc', 'vix', 'gold', 'sse', 'ftse')
write.csv(x, '/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageAllControl.csv')

ft <- read.csv('/Users/vir/Downloads/Thesis/results/ftdatascaled.csv')
scmp <- read.csv('/Users/vir/Downloads/Thesis/results/scmpdatascaled.csv')
alld <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklylnReturnAllControldata.csv')
dsvi <- read.csv2('/Users/vir/Downloads/Thesis/control_data/google_trends_daily_adjusted.csv',
                 sep = ';',
                 header = TRUE) 
svi <- read.csv('/Users/vir/Downloads/Thesis/data/svi/allweeklyscaled.csv')
nn <- ft
nn$logPrice <- log(wbtc$weeklymean)
nn$scmpnewsVol <- scmp$newsVol
nn$scmpabnPos <- scmp$abnPos
nn$X <- NULL
nn$source <- NULL
nn$vixReturn <- NULL
nn$goldReturn <- NULL
nn$ftseReturn <- NULL
nn$logReturn <- NULL
nn$sse <- scmp$ssa
nn$wsvi <- scale(svi$globsvi)

mcor <- as.matrix(round(cor(nn, use = "pairwise.complete.obs"), 2))
stargazer(mcor, type = 'html', out='/Users/vir/Downloads/cor.htm',digits=1)
stargazer(nn, type = 'html', out='/Users/vir/Downloads/sum.htm',digits=2)

alld$FTnewsVol <- ft$newsVol
alld$FTabnPos <- ft$abnPos
alld$SCMPnewsVol <- scmp$newsVol
alld$SCMPabnPos <- scmp$abnPos
x <- as.matrix(round(cor(alld, use = "pairwise.complete.obs"), 2))
stargazer(x, type = 'html', out='/Users/vir/Downloads/cor2.htm',digits=1)

# ft
ft$logPrice <- log(wbtc$weeklymean)

withmark <- read.csv('/Users/vir/Downloads/Thesis/ft/negwordpercent.csv')
ft$against <- withmark$against
ft$laundering <- withmark$laundering
ft$crisis <- withmark$crisis
ft$lost <- withmark$lost
ft$late <- withmark$late

m1 <- lm(logPrice~newsVol + gold + vix + ftse, data = ft)
m2 <- lm(logPrice~against + laundering + crisis + lost + late, data = ft)
summary(m2)
m3 <- lm(logPrice~abnPos + newsVol + gold + vix + ftse, data = ft)

# scmp
scmp$logPrice <- log(wbtc$weeklymean)
m21 <- lm(logPrice~newsVol + gold + vix + ssa, data = scmp)
m22 <- lm(logPrice~abnPos + gold + vix + ssa, data = scmp)
m23 <- lm(logPrice~abnPos + newsVol + gold + vix + ssa, data = scmp)

# Heteroskedasticity Breusch-Pagan test
library('lmtest')
lmtest::bgtest(m1)
# NCV test
car::ncvTest(m1)
library('sandwich')
stargazer(coeftest(m1, vcovHC), coeftest(m2, vcovHC), coeftest(m3, vcovHC),coeftest(m21, vcovHC),
          type = 'html', out = '/Users/vir/Downloads/corrected.htm')

library('stargazer')
stargazer(m1, m2, m3, m21, m22, m23, mm1, mm2, mm3,
          type = 'html', out = '/Users/vir/Downloads/output.htm')

library('stargazer')
stargazer(n1, n2, n3, n21, n22, n23, 
          type = 'html', out = '/Users/vir/Downloads/output2.htm')

