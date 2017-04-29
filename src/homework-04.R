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

table(tweets$handle)

demrepcol <- c("#232066" , "#E91D0E")

ggplot(tweets,aes(handle,fill = handle)) + 
  geom_bar(stat = "count") +
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

langhaszn <- subset(langhaszn,(langhaszn$Var2 == "en")|(langhaszn$Var2 == "es"))

#külön változóban kell használni a tweetek számát, mert a position dodge-ot
#nem lehet rábírni arra, hogy Trump oszlopát ne szélesítse ki végig.
#a http://ggplot2.tidyverse.org szerint van egy olyan, hogy preserve = "single"
# argumentum a position_dodge-ban , de itt nem akar működni, és a helpben sincs
# ilyenről szó. Minden up-to-date.

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

source("~/homework-04/src/homework-04-functions.R")

tweetek_csokkeno("Hillary Clinton",10)
tweetek_csokkeno("Donald Trump",15)
tweetek_csokkeno("kalap")
tweetek_csokkeno()

#### 3/1 ####

if (!("fivethirtyeight" %in% installed.packages())) {
  install.packages("fivethirtyeight")
}

library(fivethirtyeight)
data("hiphop_cand_lyrics")

View(hiphop_cand_lyrics)


ggplot(hiphop_cand_lyrics,
       aes(album_release_date, fill = candidate)) +
  geom_bar(stat = "count", position = position_stack())

# képtelen egymásra rakni a stackeket, itt is megint kerülőútra van szükség

hiphiphurra <- as.data.frame(table(hiphop_cand_lyrics$candidate,
                                   hiphop_cand_lyrics$album_release_date))

ggplot(hiphiphurra,aes(Var2,Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = position_stack()) +
  xlab("") +
  ylab("") +
  ggtitle("Every mention of 2016 primary candidates in hip-hop songs") +
  theme(plot.title = element_text(hjust = 0),
        panel.grid.major = element_line(colour = "darkgrey"),
        panel.grid.minor = element_line(colour = "darkgrey"),
        panel.background = element_rect(fill = "lightgrey",colour = "lightgrey"),
        plot.background = element_rect(fill = "lightgrey"),
        legend.position = "top",
        legend.background = element_rect(fill ="lightgrey")
        ) +
  scale_x_discrete(labels = c("1990" = "1990", "1995" = "'95","2000" = "2000",
                              "2005" = "'05","2010" = "'10","2015" = "'15"),
                   breaks = c(1990,1995,2000,2005,2010,2015))

ggsave("fig/hiphop1.png")

hiphiphurra2 <- as.data.frame(table(hiphop_cand_lyrics$candidate,
                                    hiphop_cand_lyrics$album_release_date,
                                    hiphop_cand_lyrics$sentiment))
hiphiphurra2 <- hiphiphurra2[hiphiphurra2$Var3 == "positive",]

ggplot(hiphiphurra2,aes(Var2,Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = position_stack()) +
  xlab("") +
  ylab("") +
  ggtitle("Positive") +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(colour = "darkgrey"),
        panel.grid.minor = element_line(colour = "darkgrey"),
        panel.background = element_rect(fill = "lightgrey",colour = "lightgrey"),
        plot.background = element_rect(fill = "lightgrey"),
        legend.position = "top",
        legend.background = element_rect(fill ="lightgrey")
  ) +
  scale_x_discrete(labels = c("1990" = "1990", "1995" = "'95","2000" = "2000",
                              "2005" = "'05","2010" = "'10","2015" = "'15"),
                   breaks = c(1990,1995,2000,2005,2010,2015))

ggsave("fig/hiphop2.png")


#### 3/2 #### 


#### 4/1 ####


