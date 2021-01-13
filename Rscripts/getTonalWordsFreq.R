#fq <- read.csv('/Users/vir/Downloads/Thesis/scmp/wordfrequency')

fq <- read.csv('/Users/vir/Downloads/Thesis/ft/wordfrequency')

fq <- read.csv('/Users/vir/Downloads/Thesis/allnews/wordfrequency')
master <- read.csv('/Users/vir/Downloads/LoughranMcDonald_MasterDictionary_2018.csv')

fq <- fq[rev(order(fq$count)), ]

pos <- NULL
neg <- NULL
for (i in 1:2000)
{
  index <- which(fq$word[i] == master$Word)
  if (length(index) != 0)
  {
    if (master$Negative[index] != 0)
      neg <- rbind(neg, fq[i, ])

    if (master$Positive[index] != 0)
      pos <- rbind(pos, fq[i, ])
  }
}

neg[1:10,]
all <- cbind(neg[1:30, ], pos[1:30,])
library('stargazer')
stargazer(all, type = 'html', out='/Users/vir/Downloads/wordcount.htm', summary = FALSE)

