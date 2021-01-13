cnsvi <- read.csv('/Users/vir/Downloads/Thesis/data/svi/dailychina.csv')
uksvi <- read.csv('/Users/vir/Downloads/Thesis/data/svi/dailygb.csv')
gsvi <- read.csv2('/Users/vir/Downloads/Thesis/control_data/google_trends_daily_adjusted.csv', header = TRUE,
                  sep = ';')
all <- read.csv('/Users/vir/Downloads/Thesis/data/daily/allprices.csv')
all$vix <- scale(all$vix)
all$gold <- scale(all$gold)
all$ftse <- scale(all$ftse)
all$sse <- scale(all$sse)

btc <- read.csv('/Users/vir/Downloads/Thesis/control_data/btcyahoo.csv')
btc <- btc[as.character(btc$Date) >= '2013-04-01' & as.character(btc$Date) <= '2019-07-01', ]
all$btc <- btc$Adj.Close[1:2282]

cnsvi$factors <- scale(cnsvi$factors)
cnall <- cbind(all, cnsvi$factors, 'cn')
uksvi$factors <- scale(uksvi$factors)
ukall <- cbind(all, uksvi$factors, 'uk')
colnames(cnall)[7:8] <- c('svi', 'country')
colnames(ukall)[7:8] <- c('svi', 'country')
pdata <- rbind(cnall, ukall)
write.csv(pdata, '/Users/vir/Downloads/Thesis/data/daily/panal.csv', row.names = FALSE)

pdata <- read.csv('/Users/vir/Downloads/Thesis/data/daily/panal.csv')
btc <- read.csv('/Users/vir/Downloads/Thesis/control_data/btcyahoo.csv')
btc1 <- btc[as.character(btc$Date) >= '2013-04-01' & as.character(btc$Date) <= '2019-06-30', ]
btc2 <- btc[as.character(btc$Date) >= '2013-04-02' & as.character(btc$Date) <= '2019-07-01', ]
btc3 <- btc[as.character(btc$Date) >= '2013-04-03' & as.character(btc$Date) <= '2019-07-02', ]
btc7 <- btc[as.character(btc$Date) >= '2013-04-07' & as.character(btc$Date) <= '2019-07-08', ]
btc10 <- btc[as.character(btc$Date) >= '2013-04-10' & as.character(btc$Date) <= '2019-07-11', ]
pdata$btc[1:2282] <- log(btc1$Adj.Close)
pdata$btc[2283:4564] <- log(btc1$Adj.Close)

pdata$btc2[1:2282] <- log(btc2$Adj.Close)
pdata$btc2[2283:4564] <- log(btc2$Adj.Close)

pdata$btc3[1:2282] <- log(btc3$Adj.Close)
pdata$btc3[2283:4564] <- log(btc3$Adj.Close)

pdata$btc7[1:2282] <- log(btc7$Adj.Close)
pdata$btc7[2283:4564] <- log(btc7$Adj.Close)

pdata$btc10[1:2282] <- log(btc10$Adj.Close)
pdata$btc10[2283:4564] <- log(btc10$Adj.Close)

nn <- NULL
nn$cnsvi <- cnsvi$factors
nn$uksvi <- uksvi$factors
nn$gold <- all$gold
nn$vix <- all$vix
nn$ftse <- all$ftse
nn$sse <- all$sse
mcor <- as.matrix(round(cor(data.frame(nn), use = "pairwise.complete.obs"), 2))
stargazer(mcor, type = 'html', out='/Users/vir/Downloads/cor.htm',digits=1)

library('car')
m4 <- lm(btc~svi*country + vix + gold + ftse + sse, data = pdata)
m5 <- lm(btc2~svi*country + vix + gold + ftse + sse, data = pdata)
m6 <- lm(btc3~svi*country + vix + gold + ftse + sse, data = pdata)
m7 <- lm(btc7~svi*country + vix + gold + ftse + sse, data = pdata)
m8 <- lm(btc10~svi*country + vix + gold + ftse + sse, data = pdata)
stargazer(m4, m5, m6, m7, m8,
          type = 'html', out='/Users/vir/Downloads/dsvi.htm')
round(vif(m4),2)
coeftest(m4, vcovHC)

