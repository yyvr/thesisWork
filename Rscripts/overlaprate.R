srate <- read.table('/Users/vir/Downloads/Thesis/data/scmpoverlaprates', header = FALSE, sep = ',')
frate <- read.table('/Users/vir/Downloads/Thesis/data/ftoverlaprates', header = FALSE, sep = ',')

srate <- t(srate)
frate <- t(frate)
t_col <- function(color, percent = 50, name = NULL) {
  rgb.val <- col2rgb(color)
  ## Make new color using input color as base and alpha set by transparency
  t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
               max = 255,
               alpha = (100-percent)*255/100,
               names = name)
  ## Save the color
  invisible(t.col)
}
mycol <- t_col('red', perc = 80, name = 'lt.red')
mycol2 <- t_col('blue', perc = 40, name = 'lt.blue')
hist(frate, col = 'red', xlim=c(0, 1), ylim=c(0, 600), xlab = 'Overlap rate', ylab = 'Number of news', 
     main = 'Histogram of vocabulary overlapping rate')
hist(srate, add = T, col = mycol2)
legend("topleft", legend=c("FT", "SCMP"), 
       col=c("red", mycol2), fill=c("red", mycol2), cex=0.8)
