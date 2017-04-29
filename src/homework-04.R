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

#### 2/2 ####

tweetnumber<-as.data.frame(table(tweets$handle))

demrepcol <- c("#232066" , "#E91D0E")

ggplot(tweetnumber,aes(Var1,Freq,fill = Var1)) + 
  geom_bar(stat = "identity") +
  ggtitle("Candidate Tweets") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = NULL)) +
  ylab("Tweet Frequency") +
  xlab("") +
  theme(panel.background = element_rect(fill = "white", colour = NULL)) +
  scale_fill_manual(values=demrepcol)

ggsave("fig/tweet1.png")


#### 2/3 ####

table(tweets$handle,tweets$lang)

noneng <- subset(tweets, (tweets$lang != "en") & (tweets$lang != "es"))

#View(noneng)

tweets$lang[tweets$lang != "en" & tweets$lang != "es"] <- "en"

# Hillary nem angol és nem spanyol tweetjei javítva

noneng2<- subset(tweets, (tweets$handle == "realDonaldTrump") &
                         (tweets$lang != "en"))

#View(noneng2)

tweets$lang[tweets$lang == "es" & tweets$handle == "realDonaldTrump"] <- "en"

# Trump spanyol tweetjei javítva

langhaszn <- as.data.frame(table(tweets$handle,tweets$lang))

langhaszn <- subset(langhaszn,( langhaszn$Var2 == "en") | (langhaszn$Var2 == "es"))

palette2 <- c("darkgrey","cornflowerblue")

ggplot(langhaszn, aes(Var1,Freq, group = Var2, fill = Var2  )) + 
  geom_bar(stat = "identity",position = position_dodge()) +
  ggtitle("Language of Tweets") +
  xlab("") +
  ylab("Tweet Frequency") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "white", colour = NULL)) +
  scale_fill_manual(values=palette2)
  
ggsave("fig/tweet2.png")

#### 2/4 ####

tweetek_csokkeno("Hillary Clinton",10)
tweetek_csokkeno("Donald Trump",15)
tweetek_csokkeno("kalap")
tweetek_csokkeno()
