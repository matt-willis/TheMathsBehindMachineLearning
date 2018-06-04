library(ggplot2)
library(plyr)
library(cluster)

#Import dataset and clean dataset. Please change your path to the source file below. :)
setwd("C:\\Users\\mdw.ACL\\OneDrive - Adatis\\Conferences\\SQL Plovdiv\\The Maths Behind Machine Learning")
dfMusic <- read.csv("music.csv")
dfMusic <- na.omit(dfMusic[!dfMusic$year==0,!names(dfMusic) %in% "song.hotttnesss", drop = F])

#Find the optimal value of k using the elbow method.
optimalKElbow <- function(k) {
  musicCluster <- kmeans((dfMusic$duration + dfMusic$mode + dfMusic$mode_confidence + dfMusic$key + dfMusic$key_confidence +
                       dfMusic$start_of_fade_out + dfMusic$end_of_fade_in + dfMusic$time_signature +
                       dfMusic$time_signature_confidence + dfMusic$loudness), k)
  return (musicCluster$tot.withinss)
}
oke <- sapply(2:20, optimalKElbow)
elbow <- data.frame(2:20, oke)

# Plot the elbow graph.
ggplot(elbow, aes(x = X2.20, y = oke)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))

#Create the clustering model and assign the cluster to each row.
musicCluster <- kmeans((dfMusic$duration + dfMusic$mode + dfMusic$mode_confidence + dfMusic$key + dfMusic$key_confidence +
                          dfMusic$start_of_fade_out + dfMusic$end_of_fade_in + dfMusic$time_signature +
                          dfMusic$time_signature_confidence + dfMusic$loudness),5, nstart = 25)
dfMusic$cluster <- musicCluster$cluster

#Create a data frame containing some of my favourite artists.
favouriteArtist <- c("Radiohead", "The Velvet Underground", "Bob Dylan", "The Smashing Pumpkins", "Gorillaz", "The Clash")
dfMusicFavouriteArtist <- subset(dfMusic, artist.name %in% favouriteArtist)

#Create data frame with my preferred cluster.
topClusters <- count(dfMusicFavouriteArtist$cluster, c('dfMusicFavouriteArtist$cluster'))
topClusters <- topClusters[order(topClusters$freq,decreasing=TRUE), ]
favouriteCluster <- dfMusic[dfMusic$cluster==topClusters[1,1],]