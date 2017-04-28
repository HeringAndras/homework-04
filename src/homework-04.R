# Házi feladat 4
# Programozás I.
# 2016/17. II. félév
# Hering András
# 2017.04.28

if (!("ggplot2" %in% installed.packages())) {
  install.packages("ggplot2")
}

library(ggplot2)

#### 2/1 ####

tweets <- read.csv(file = "~/homework-04/data/clinton_trump_tweets.csv",
                     header = T ,sep = ";")

tweetnumber<-as.data.frame(table(tweets$handle))

demrepcol <- c("#232066" , "#E91D0E")

ggplot(tweetnumber,aes(Var1,Freq, col = Var1)) + geom_bar(stat = "identity",fill = demrepcol)

