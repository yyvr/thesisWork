cdata <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklyAverageAllControl.csv')

ft <- read.csv('/Users/vir/Downloads/Thesis/results/ftdatascaled.csv')
scmp <- read.csv('/Users/vir/Downloads/Thesis/results/scmpdatascaled.csv')
alld <- read.csv('/Users/vir/Downloads/Thesis/data/weekly/weeklylnReturnAllControldata.csv')
svi <- read.csv('/Users/vir/Downloads/Thesis/data/svi/allweeklyscaled.csv')

nn <- ft
nn$scmpnewsVol <- scmp$newsVol
nn$scmpabnPos <- scmp$abnPos
nn$X <- NULL
nn$sse <- scmp$ssa
nn$wsvi <- scale(svi$globsvi)
nn$lnPrice2 <- NULL

mcor <- as.matrix(round(cor(nn, use = "pairwise.complete.obs"), 2))
stargazer(mcor, type = 'html', out='/Users/vir/Downloads/cor.htm',digits=1)
stargazer(nn, type = 'html', out='/Users/vir/Downloads/sum.htm',digits=2)

ft$source <- 'FT'
scmp$source <- 'SCMP'
ft$ssa <- scmp$ssa
scmp$ftse <- ft$ftse
data <- rbind(ft, scmp)
data$source <- as.factor(data$source)

# ft
m1 <- lm(lnPrice1~newsVol + gold + vix + ftse, data = ft)
m2 <- lm(lnPrice1~abnPos + gold + vix + ftse, data = ft)
m3 <- lm(lnPrice1~abnPos + newsVol + gold + vix + ftse, data = ft)

# scmp
m21 <- lm(lnPrice1~newsVol + gold + vix + ssa, data = scmp)
m22 <- lm(lnPrice1~abnPos + gold + vix + ssa, data = scmp)
m23 <- lm(lnPrice1~abnPos + newsVol + gold + vix + ssa, data = scmp)

mm1 <- lm(lnPrice1~abnPos + gold + ftse + ssa + vix + source, 
          data = data)
mm2 <- lm(lnPrice1~newsVol + gold + ftse + ssa + vix + source, 
          data = data)
mm3 <- lm(lnPrice1~abnPos + newsVol + gold + ftse + ssa + vix + source, 
          data = data)
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
          type = 'html', out = '/Users/vir/Downloads/output2.htm')

library('stargazer')
stargazer(n1, n2, n3, n21, n22, n23, 
          type = 'html', out = '/Users/vir/Downloads/output2.htm')

