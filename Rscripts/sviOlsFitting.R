library('car')
svi <- read.csv2('/Users/vir/Downloads/Thesis/control_data/google_trends_daily_adjusted.csv',
                 header=TRUE, 
                 sep=";")
data <- read.csv('/Users/vir/Downloads/Thesis/data/adjusted_price_sent_stoxx_gold.csv')
scmpdata <- read.csv('/Users/vir/Downloads/Thesis/data/scmp_with_controlv.csv')
svi <- svi[svi$date >= '2013-04-22', ]
wsvi <- NULL
for (i in seq(1, 2280, 7))
{
  wsvi <- c(wsvi, sum(svi$adjusted_hists[i:(i+6)]))
}
wsvi <- wsvi[1:323]/100
twsvi <- caret::BoxCoxTrans(wsvi)

data.sel <- subset(data, select = c(vix, gold, price, ftse_gbp))
data.sel <- cbind(wsvi, data.sel)
colnames(data.sel) <- c('wsvi','vix', 'gold', 'logPrice', 'ftse')
data.sel$logPrice <- log10(data$price)
data.sel$ftse <- data$ftse_gbp/1000
data.sel$ssa <- scmpdata$ssa_cny/1000
data.sel$twsvi <- predict(twsvi, wsvi)

fullfit <- lm(logPrice~vix +  gold + ftse + twsvi, 
              data = data.sel)
summary(fullfit)
fullfit2 <- lm(logPrice~wsvi + vix +  gold + ssa, 
              data = data.sel)
vvf <- round(unname(vif(fullfit)), 2)
vvf2 <- round(unname(vif(fullfit2)), 2)
ff<- summary(fullfit)
ff <- rbind(round(ff$coefficients, 2), c(round(ff$r.squared, 2), ' ', ' ',''), 
            c(round(ff$adj.r.squared, 2), ' ', ' ',''))
zz <- cbind(ff[, c(1, 3,4)], c('', vvf, '',''))

# add two rows fro R^2 and adj.R^2
rownames(zz) <- c(rownames(ff)[1:(nrow(ff) - 2)], 'R^2', 'Adj. R^2')

ff2<- summary(fullfit2)
ff2 <- rbind(round(ff2$coefficients, 2), c(round(ff2$r.squared, 2), ' ', ' ',''), 
             c(round(ff2$adj.r.squared, 2), ' ', ' ',''))

# generate table for the results
zz <- cbind(zz, c(rownames(ff2)[1:(nrow(ff2) - 2)], 'R^2', 'Adj. R^2'))
zz <- cbind(zz, ff2[, c(1,3,4)], c('', vvf2, '', ''))
write('Model using FTSE,,,,,Model using SSA\n,Coefficient,t-value,p-value,VIF,,Coefficient,t-value,p-value, VIF', '/Users/vir/Downloads/fitresultSVI.csv')
write.table(zz, '/Users/vir/Downloads/fitresultSVI.csv', 
            append = TRUE, sep = ",", quote = FALSE, col.names = FALSE)

mcor <- as.matrix(round(cor(data.sel[, c(2:7)], use = "pairwise.complete.obs"), 2))
upper<-mcor
upper[upper.tri(mcor)]<-""
upper<-as.data.frame(upper)
upper

set.seed(2019)
train.size <- 0.8 
train.index <- sample.int(length(data.sel$logPrice), 
                          round(length(data.sel$logPrice) * train.size))
train.sample <- data.sel[train.index,]
valid.sample <- data.sel[-train.index,]

trainingfit <- lm(logPrice~wsvi + stoxx + vix + ftse + gold + ssa, 
              data = train.sample)
validfit <- lm(logPrice~wsvi + stoxx + vix + ftse + gold + ssa, 
              data = valid.sample)
fullfit <- lm(logPrice~wsvi + stoxx + vix +  gold + ssa, 
          data = data.sel)

tt <- summary(trainingfit)
vv <- summary(validfit)
ff <- summary(fullfit)

tt <- rbind(round(tt$coefficients, 2), c(round(tt$r.squared, 2), ' ', ' ',''), 
            c(round(tt$adj.r.squared, 2), ' ', ' ',''))
vv <- rbind(round(vv$coefficients, 2), c(round(vv$r.squared, 2), ' ', ' ',''), 
            c(round(vv$adj.r.squared, 2), ' ', ' ',''))
ff <- rbind(round(ff$coefficients, 2), c(round(ff$r.squared, 2), ' ', ' ',''), 
            c(round(ff$adj.r.squared, 2), ' ', ' ',''))
zz <- cbind(tt[, c(1, 3, 4)], vv[, c(1, 3,4)], ff[, c(1, 3,4)])
rownames(zz) <- c(rownames(tt)[1:7], 'R^2', 'Adjusted R^2')
write(',Training Set,,,Validation Set,,,Full Set', '/Users/vir/Downloads/fitresultSVI.csv')
write(',Coefficient,t-value,p-value,Coefficient,t-value,p-value,Coefficient,t-value,p-value', 
      '/Users/vir/Downloads/fitresultSVI.csv', append = TRUE)
write.table(zz, '/Users/vir/Downloads/fitresultSVI.csv', 
            append = TRUE, sep = ",", quote = FALSE, col.names = FALSE)

lmtest::bgtest(fullfit)
# NCV test
car::ncvTest(fullfit)


fit <- lm(log_price~recip_wsvi + recip_stoxx_usd + gold, 
          data = train.sample)
summary(fit)
crPlots(fit)

a <- vif(fit)
write.table(t(round(a, 2)), '/Users/vir/Downloads/vifresultSVI.csv')

train.sample$Pred.Price <- predict(fit, newdata = train.sample)
valid.sample$Pred.Price <- predict(fit, newdata = valid.sample)

train.corr <- round(cor(train.sample$Pred.Price, train.sample$log_price), 2)
train.RMSE <- round(sqrt(mean((10 ^ train.sample$Pred.Price - 10 ^ train.sample$log_price)^2)))
train.MAE <- round(mean(abs(10 ^ train.sample$Pred.Price - 10 ^ train.sample$log_price)))
c(train.corr^2, train.RMSE, train.MAE)

# Check how good is the model on the validation set - correlation^2, RME and MAE
valid.corr <- round(cor(valid.sample$Pred.Price, valid.sample$log_price), 2)
valid.RMSE <- round(sqrt(mean((10 ^ valid.sample$Pred.Price - 10 ^ valid.sample$log_price)^2)))
valid.MAE <- round(mean(abs(10 ^ valid.sample$Pred.Price - 10 ^ valid.sample$log_price)))
c(valid.corr^2, valid.RMSE, valid.MAE)

fit <- lm(log_price~recip_wsvi + recip_stoxx_usd + recip_vix + log_ftse_usd + gold, 
          data = data.sel)
summary(fit)
b <- summary(fit)
write.table(round(b$coefficients, 2), '/Users/vir/Downloads/fitresultSVI.csv')
