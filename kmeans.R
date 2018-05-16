### k-means 알고리즘을 이용해서 메이저리그 타자 분류해보기!
library(dplyr)
library(NbClust)

# rm(list=ls())

# data load
batting <- read.csv("Batting2010.csv")
# pitching <- read.csv("Pitching2010.csv")

# check the structure
str(batting)
# str(pitching)

# filter the data 
batting2016 <- filter(batting, yearID==2016, AB>300)
# pitching2016 <- filter(pitching, yearID==2016, IPouts>100)

# select some field
bat <- batting2016[,c("R","H","X2B","X3B","HR","SB","CS","BB","SO","SH","SF","GIDP")]
# pit <- pitching2016[,c("H", "HR", "SO", "BB", "R", "BAOpp", "ERA", "BFP")]

# check the outlier and summary data
summary(bat)

# normalize
bat <- as.data.frame(lapply(bat, scale))
# pit <- as.data.frame(lapply(pit, scale))

# check the cluster "k" number
nc <- NbClust(bat, min.nc=2, max.nc=15, method="kmeans")
# nc <- NbClust(pit, min.nc=2, max.nc=15, method="kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")

wssplot <- function(data, nc=30, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}
wssplot(bat)

# execute k-means algorithm
set.seed(1027)  # fix the random
bat_cluster <- kmeans(bat, 3) # k-means
bat_cluster$size   # check the cluster size
bat_cluster$centers  # check the cluster centroid
batting2016$class <- bat_cluster$cluster # attach cluster to original data

# draw plot
plot(df, col=bat_cluster$cluster)
points(bat_cluster$centers, col=1:3, pch=8, cex=1.5)


# set.seed(1027)
# pit_cluster <- kmeans(pit, 3)
# pit_cluster$size
# pit_cluster$centers
# 
# pitching2016$cluster_size <- pit_cluster$size

