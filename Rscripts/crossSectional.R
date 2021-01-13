# ols fitting full FT data 
ft <- read.csv('/Users/vir/Downloads/Thesis/results/ftdatascaled.csv')
scmp <- read.csv('/Users/vir/Downloads/Thesis/results/scmpdatascaled.csv')

ft$source <- 'FT'
scmp$source <- 'SCMP'

ft$ssa <- scmp$ssa
ft$ssaReturn <- scmp$ssaReturn
scmp$ftse <- ft$ftse
scmp$ftseReturn <- ft$ftseReturn
data <- rbind(ft, scmp)
data$source <- as.factor(data$source)

#data$lnPrice <- log(wbtc$weeklymean)
data$lnPrice <- c(log(wbtc$weeklymean)[2:323], 9.322273)
library("xlsx")
#write.xlsx(ndata, '/Users/vir/Downloads/Thesis/results/regressdata.xlsx', 
#           sheetName = "CrossSectional", 
#           col.names = TRUE, row.names = TRUE, append = FALSE)
mm1 <- lm(lnPrice~abnPos + gold + ftse + ssa + vix + source, 
            data = data)
mm2 <- lm(lnPrice~newsVol + gold + ftse + ssa + vix + source, 
              data = data)
mm3 <- lm(lnPrice~abnPos + newsVol + gold + ftse + ssa + vix + source, 
            data = data)

library(car)
a <- scatterplot(ft$abnPos, ft$logPrice,
                 ylab="Relative news volume", xlab="Abnormal positivity",
                 smooth=list(smoother=loessLine, var=TRUE, col.smooth = 'red', col.var = 'dodgerblue4', 
                             lty.var=4, # type of variance smooth line
                             lty.smooth=1)) # type of mean smooth line
