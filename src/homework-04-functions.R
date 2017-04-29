#### functions ####

tweetek_csokkeno <- function(name = "Donald Trump",count = 3) {
    if(name == "Hillary Clinton") {
      szumcount <- subset(tweets,tweets$handle == "HillaryClinton")
    } else if(name == 'Donald Trump') {
      szumcount <- subset(tweets,tweets$handle == "realDonaldTrump")
    } else {
      stop("név hiba")
    }
  szumcount$szum <- apply(szumcount[,c(12,13)],1,sum)
  szumcount <- szumcount[order(szumcount$szum,decreasing = T),]
  if (count <= nrow(szumcount)){
    View(szumcount[c(1:count),])
    paste("Top",count,"tweetje",name,"-nak")
  } else {stop(
    paste("válassz kevesebb tweetet,max tweetek száma ennél a tagnál:",
          nrow(szumcount)))}
}



