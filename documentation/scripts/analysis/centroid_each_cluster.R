# SPATIOTEMPORAL
n <- max(dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster) + 1
dataframe_spatiotemporal_max10 <- NA
dataframe_spatiotemporal_max10 <- data.frame(1, 2.2, 2.2, 344 , 0.4, 0.5, as.Date("2012/12/12",format="%Y/%m/%d") ,as.Date("2012/12/12",format="%Y/%m/%d"))
colnames(dataframe_spatiotemporal_max10) <- c("cluster", "x", "y", "p","literacyRate","workerPop_main","event_start","event_end")
for(i in 0:(n-1)){
  xy <- as.vector(gCentroid(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster==i,])@coords)
  p <- mean(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster==i,]$p)
  event_date <- mean(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster==i,]$event_start)
  literacyRate <- mean(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster==i,]$literacyRate)
  workerPop_main <- mean(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster==i,]$workerPop_main)
  	dataframe_spatiotemporal_max10[i+1,"cluster"] <- i
	dataframe_spatiotemporal_max10[i+1,"x"] <- xy[1]
	dataframe_spatiotemporal_max10[i+1,"y"] <- xy[2]
	dataframe_spatiotemporal_max10[i+1,"p"] <- p
	dataframe_spatiotemporal_max10[i+1,"literacyRate"] <- literacyRate
	dataframe_spatiotemporal_max10[i+1,"workerPop_main"] <- workerPop_main
	dataframe_spatiotemporal_max10[i+1,"event_start"] <- event_date
	dataframe_spatiotemporal_max10[i+1,"event_end"] <- event_date
}
centroid_spatiotemporal_max10 <- dataframe_spatiotemporal_max10
coordinates(centroid_spatiotemporal_max10)=~x+y
crs(centroid_spatiotemporal_max10)="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# SOCIOECONOMIC
n <- max(dbscan_distanceMatrix_india_2014_northeast_socioeconomic_max10$cluster) + 1
dataframe_socioeconomic_max10 <- NA
dataframe_socioeconomic_max10 <- data.frame(1, 2.2, 2.2, 344 , 0.4, 0.5, as.Date("2012/12/12",format="%Y/%m/%d") ,as.Date("2012/12/12",format="%Y/%m/%d"))
colnames(dataframe_socioeconomic_max10) <- c("cluster", "x", "y", "p","literacyRate","workerPop_main","event_start","event_end")
for(i in 0:(n-1)){
  xy <- as.vector(gCentroid(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_socioeconomic_max10$cluster==i,])@coords)
  p <- mean(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_socioeconomic_max10$cluster==i,]$p)
  event_date <- mean(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_socioeconomic_max10$cluster==i,]$event_start)
  literacyRate <- mean(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_socioeconomic_max10$cluster==i,]$literacyRate)
  workerPop_main <- mean(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_socioeconomic_max10$cluster==i,]$workerPop_main)
  	dataframe_socioeconomic_max10[i+1,"cluster"] <- i
	dataframe_socioeconomic_max10[i+1,"x"] <- xy[1]
	dataframe_socioeconomic_max10[i+1,"y"] <- xy[2]
	dataframe_socioeconomic_max10[i+1,"p"] <- p
	dataframe_socioeconomic_max10[i+1,"literacyRate"] <- literacyRate
	dataframe_socioeconomic_max10[i+1,"workerPop_main"] <- workerPop_main
	dataframe_socioeconomic_max10[i+1,"event_start"] <- event_date
	dataframe_socioeconomic_max10[i+1,"event_end"] <- event_date
}
centroid_socioeconomic_max10 <- dataframe_socioeconomic_max10
coordinates(centroid_socioeconomic_max10)=~x+y
crs(centroid_socioeconomic_max10)="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

# COMBINED
n <- max(dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster) + 1
dataframe_combined_max10 <- NA
dataframe_combined_max10 <- data.frame(1, 2.2, 2.2, 344 , 0.4, 0.5, as.Date("2012/12/12",format="%Y/%m/%d") ,as.Date("2012/12/12",format="%Y/%m/%d"))
colnames(dataframe_combined_max10) <- c("cluster", "x", "y", "p","literacyRate","workerPop_main","event_start","event_end")
for(i in 0:(n-1)){
  xy <- as.vector(gCentroid(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster==i,])@coords)
  p <- mean(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster==i,]$p)
  event_date <- mean(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster==i,]$event_start)
  # print(event_date)
  literacyRate <- mean(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster==i,]$literacyRate)
  workerPop_main <- mean(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster==i,]$workerPop_main)
  	dataframe_combined_max10[i+1,"cluster"] <- i
	dataframe_combined_max10[i+1,"x"] <- xy[1]
	dataframe_combined_max10[i+1,"y"] <- xy[2]
	dataframe_combined_max10[i+1,"p"] <- p
	dataframe_combined_max10[i+1,"literacyRate"] <- literacyRate
	dataframe_combined_max10[i+1,"workerPop_main"] <- workerPop_main
	dataframe_combined_max10[i+1,"event_start"] <- event_date
	dataframe_combined_max10[i+1,"event_end"] <- event_date
}
centroid_combined_max10 <- dataframe_combined_max10
coordinates(centroid_combined_max10)=~x+y
crs(centroid_combined_max10)="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#CENTROIDS
# (centroid_spatiotemporal_max10)
# (centroid_socioeconomic_max10)
# (centroid_combined_max10)
