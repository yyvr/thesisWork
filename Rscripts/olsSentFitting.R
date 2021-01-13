# ols fitting FT data
data <- read.csv('/Users/vir/Downloads/Thesis/data/adjusted_price_sent_stoxx_gold.csv')
data.sel <- subset(data, select = c(price, count, abn_pos, vix, gold, ftse_gbp))
colnames(data.sel) <- c('logPrice', 'count', 'abnPos', 'vix', 'gold', 'ftse')

data.sel$logPrice <- log10(data$price)
data.sel$count <- data$count
data.sel$abnPos <- data$abn_pos
data.sel$ftse <- data$ftse_gbp
#data.sel$ftse <- data$ftse_gbp
#data.sel$recipVix <- 100/data$vix
#data.sel$stoxx <- data$stoxx_euro

fullfit <- lm(logPrice~abnPos + count + ftse + gold + vix, 
              data = data.sel)
summary(fullfit)
vif(fullfit)

# We will use (train.size)% for training and (100-train.size)% for validation
set.seed(2019)
train.size <- 0.8 
train.index <- sample.int(length(data.sel$logPrice), 
                          round(length(data.sel$logPrice) * train.size))
train.sample <- data.sel[train.index,]
valid.sample <- data.sel[-train.index,]

trainingfit <- lm(logPrice~abnPos + count + ftse + gold + vix, 
                  data = train.sample)
validfit <- lm(logPrice~abnPos + count + ftse + gold + vix,
               data = valid.sample)
fullfit <- lm(logPrice~abnPos + count + ftse + gold + vix, 
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
write(',Training Set,,,Validation Set,,,Full Set', '/Users/vir/Downloads/fitresult.csv')
write(',Coefficient,t-value,p-value,Coefficient,t-value,p-value,Coefficient,t-value,p-value', 
      '/Users/vir/Downloads/fitresult.csv', append = TRUE)
write.table(zz, '/Users/vir/Downloads/fitresult.csv', 
            append = TRUE, sep = ",", quote = FALSE, col.names = FALSE)

a <- scatterplot(data.sel$abnPos, data.sel$count,
ylab="Relative news volume", xlab="Abnormal positivity",
smooth=list(smoother=loessLine, var=TRUE, col.smooth = 'red', col.var = 'dodgerblue4', 
            lty.var=4, # type of variance smooth line
            lty.smooth=1)) # type of mean smooth line

scatterplot(prestige ~ income | type, data=Prestige,
            smooth=list(smoother=quantregLine, var=TRUE, span=1, lwd=4, lwd.var=2))

mcor <- as.matrix(round(cor(data.sel[, c(2:6)], use = "pairwise.complete.obs"), 2))
upper<-mcor
upper[upper.tri(mcor)]<-""
upper<-as.data.frame(upper)
upper
write.table(upper, '/Users/vir/Downloads/corr.csv', 
            sep = ",", quote = FALSE, col.names = TRUE)

# Check for multi-collinearity with Variance Inflation Factor
# Correlated: none VIF=1, moderately 1<VIF<5, ** highly 5<VIF<10, ...
a <- vif(fit)
write.table(t(round(a, 4)), '/Users/vir/Downloads/vifresult.csv')

cutoff <- 4/((nrow(train.sample)-length(fit$coefficients)-2)) # Cook's D plot, cutoff as 4/(n-k-1)
plot(fit, which=4, cook.levels=cutoff)                        # identify D values > cutoff
plot(fit, which=5, cook.levels=cutoff)
train.sample <- train.sample[-which(rownames(train.sample)    # Row names discovered in 2 rounds
                                    %in% c("159", "288", "281")),] 
fit <- lm(logPrice~abnPos + sqrtCount + recipStoxx + logFtse + gold + recipVix, 
          data = train.sample)
summary(fit)

# Check for multi-collinearity with Variance Inflation Factor
# Correlated: none VIF=1, moderately 1<VIF<5, ** highly 5<VIF<10, ...
a <- vif(fit)

write.table(t(round(a, 2)), '/Users/vir/Downloads/vifresult.csv')
##### Now evaluate the final linear model
#     Find all predicted values for both a training set and a validation set
train.sample$Pred.Price <- predict(fit, newdata = train.sample)
valid.sample$Pred.Price <- predict(fit, newdata = valid.sample)

# The theoretical model performance is defined here as R-Squared
summary(fit)
b <- summary(fit)
write.table(round(b$coefficients, 2), '/Users/vir/Downloads/fitresult.csv')
# Check how good is the model on the training set - correlation^2, RME and MAE
train.corr <- round(cor(train.sample$Pred.Price, train.sample$logPrice), 2)
train.RMSE <- round(sqrt(mean((10 ^ train.sample$Pred.Price - 10 ^ train.sample$logPrice)^2)))
train.MAE <- round(mean(abs(10 ^ train.sample$Pred.Price - 10 ^ train.sample$logPrice)))
c(train.corr^2, train.RMSE, train.MAE)

# Check how good is the model on the validation set - correlation^2, RME and MAE
valid.corr <- round(cor(valid.sample$Pred.Price, valid.sample$logPrice), 2)
valid.RMSE <- round(sqrt(mean((10 ^ valid.sample$Pred.Price - 10 ^ valid.sample$logPrice)^2)))
valid.MAE <- round(mean(abs(10 ^ valid.sample$Pred.Price - 10 ^ valid.sample$logPrice)))
c(valid.corr^2, valid.RMSE, valid.MAE)
