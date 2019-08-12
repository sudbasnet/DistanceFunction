# aes(x=Events, y=value, lty=variable)) +

sapply (c("sp", "raster", "spatstat", "maptools", "plotrix", "scatterplot3d", "assertthat", "geosphere","dbscan", "rgdal", "rgeos", "stringr", "sqldf", "ggplot2", "xlsx"), install.packages)

# the shapefiles of india maps are set in a folder ICARS_clustering inside the working directory
# states
map_india_wesm_cluster_summaries_all
t <- shapefile("./shapefiles/map_india_west.shp")
map_india_northeast <- shapefile("./shapefiles/map_india_northeast.shp")
map_india_south <- shapefile("./shapefiles/map_india_south.shp")
map_india_north <- shapefile("./shapefiles/map_india_north.shp")
# districts
map_india_west_d <- shapefile("./shapefiles/map_india_west_d.shp")
map_india_northeast_d <- shapefile("./shapefiles/map_india_northeast_d.shp")
map_india_south_d <- shapefile("./shapefiles/map_india_south_d.shp")
map_india_north_d <- shapefile("./shapefiles/map_india_north_d.shp")

# next, i would like to add the population values from the census dataset to 
# all the districts based on their names
census_abstract_india_2011 <- read.csv("./pca-full.csv", sep=",", header=T)
census_abstract_india_2011 <- census_abstract_india_2011[ (toupper(census_abstract_india_2011$TRU)=='TOTAL') & (toupper(census_abstract_india_2011$Level)=='DISTRICT'),c(1,2,4,6,8,11,20,26,29)]
names(census_abstract_india_2011) <- c("State", "District", "Name", "area_km_sq","population_total", "population_age0to6", "population_literate", "population_total_worker", "population_main_workforce")

# now pull the population from the census datasets ( i can pull other variables too, but lets do that in a separate step )
# note that we are only pulling values for districts
map_india_northeast_d <- merge(map_india_northeast_d, census_abstract_india_2011[,c("District","area_km_sq","population_total")], by.x="censuscode", by.y="District", all = FALSE)
map_india_west_d <- merge(map_india_west_d, census_abstract_india_2011[,c("District", "area_km_sq", "population_total")], by.x="censuscode", by.y="District", all = FALSE)
map_india_north_d <- merge(map_india_north_d, census_abstract_india_2011[,c("District", "area_km_sq", "population_total")], by.x="censuscode", by.y="District", all = FALSE)
map_india_south_d <- merge(map_india_south_d, census_abstract_india_2011[,c("District", "area_km_sq", "population_total")], by.x="censuscode", by.y="District", all = FALSE)

# variables
variable_india_northeast_d <- merge(map_india_northeast_d, census_abstract_india_2011[,c("District", "population_age0to6", "population_literate", "population_total_worker", "population_main_workforce")], by.x="censuscode", by.y="District", all = FALSE)

# events -2014 GDELT
events_india_GDELT_2014 <- read.csv("events_india_GDELT_2014.csv",header=T, sep="," )
events_india_GDELT_2014$event_date <- as.Date(substr(events_india_GDELT_2014$event_date, 1, 10),format = "%Y-%m-%d")
events_india_2014 <- prepareEvent(events_india_GDELT_2014, lat = "latitude", lon = "longitude", tstart = "event_date", tend = "event_date", eventCategory = "event_category", url = "url", uniqueid = "event_id")

# set area files in fixed format
area_india_northeast_d <- prepareArea(map_india_northeast_d, "censuscode", "ST_NM", "DISTRICT", "population_total", "area_km_sq", "district")
area_india_west_d <- prepareArea(map_india_west_d, "censuscode", "ST_NM", "DISTRICT", "population_total", "area_km_sq", "district")
area_india_north_d <- prepareArea(map_india_north_d, "censuscode", "ST_NM", "DISTRICT", "population_total", "area_km_sq", "district")
area_india_south_d <- prepareArea(map_india_south_d, "censuscode", "ST_NM", "DISTRICT", "population_total", "area_km_sq", "district")

# events for each piece of area
events_india_2014_northeast <- events_india_2014[!is.na(sp::over(events_india_2014, map_india_northeast_d)[,1]),]
events_india_2014_west <- events_india_2014[!is.na(sp::over(events_india_2014, map_india_west_d)[,1]),]
events_india_2014_north <- events_india_2014[!is.na(sp::over(events_india_2014, map_india_north_d)[,1]),]
events_india_2014_south <- events_india_2014[!is.na(sp::over(events_india_2014, map_india_south_d)[,1]),]

# final data prepration for the distance function
data_india_2014_northeast <- prepareData(events_india_2014_northeast, area_india_northeast_d)
data_india_2014_west <- prepareData(events_india_2014_west, area_india_west_d)
data_india_2014_north <- prepareData(events_india_2014_north, area_india_north_d)
data_india_2014_south <- prepareData(events_india_2014_south, area_india_south_d)

# rgeos::over(events_india_var2014, map_india_northeast_d)

# distanceMatrix_india_2014_northeast2 <- distanceFunction_using_vectors(data_india_2014_northeast, 100, 30, var_count = 0)

# 7 April 2014 - 12 May 2014
# results announced in 16 may 2014

# q <- q + geom_point(data=as.data.frame(data_india_2014_northeast_violent@coords), aes(x=lon, y=lat), color= 'red', size=5, alpha=0.5)
# 
# q <- q + geom_point(data=as.data.frame(data_india_2014_northeast_violent@coords), aes(x=lon, y=lat), 
#                     color=dbscan_india_2014_northeast_violent$cluster, size = freq[freq$Var1 == dbscan_india_2014_northeast_violent$cluster,'Freq'], alpha=0.5)
# freq[freq$Var1 == dbscan_distanceMatrix_india_2014_northeast$cluster,'Freq']

#clustering result for northeast full data
# jpeg("clustering_northeast.jpg",3000, 3000, res=300)
# plot(map_india_northeast, xlab="DBSCAN clustering for 1770 objects, \nParameters: eps = 0.13, minPts = 4")
# plot(data_india_2014_northeast, col= dbscan_distanceMatrix_india_2014_northeast$cluster + 1, add=T)
# text(data_india_2014_northeast@coords, labels=dbscan_distanceMatrix_india_2014_northeast$cluster, cex= 0.7, col=dbscan_distanceMatrix_india_2014_northeast$cluster + 1, pos=4)
# clusters<-c(0:32)
# legend("topright",legend=clusters, col = clusters+1, pch=3)
# legend("topleft", legend="black '+'(cluster 0) are noise points")
# dev.off()
# 
# jpeg("clustering_northeast_violent.jpg",3000, 3000, res=300)
# plot(map_india_northeast, xlab="DBSCAN clustering for 320 objects, \nParameters: eps = 0.2, minPts = 4")
# plot(data_india_2014_northeast_violent, col= dbscan_india_2014_northeast_violent$cluster + 1, add=T)
# text(data_india_2014_northeast@coords, labels=dbscan_india_2014_northeast_violent$cluster, cex= 0.7, col=dbscan_india_2014_northeast_violent$cluster + 1, pos=4)
# clusters<-c(0:14)
# legend("topright",legend=clusters, col = clusters+1, pch=3)
# legend("topleft", legend="(cluster 0) are noise points")
# dev.off()

# for (i in 0:14){
#   data_india_2014_northeast_violent_loopthis <- data_india_2014_northeast_violent[dbscan_india_2014_northeast_violent$cluster == i,]
#   jpeg(paste('cluster_india_northeast_', i , '.jpg',sep=""),3000, 3000, res=300)
#   plot(map_india_northeast, main=paste("cluster: ",i))
#   plot(data_india_2014_northeast_violent_loopthis, add=T)
#   text(data_india_2014_northeast_violent_loopthis@coords, labels=paste(as.character(data_india_2014_northeast_violent_loopthis$event_start,format="%m/%d"), sep=","), cex= 0.7, pos=4)
#   dev.off()
# }
# 
# for (i in 0:32){
#   data_india_2014_northeast <- data_india_2014_northeast_violent[dbscan_india_2014_northeast_violent$cluster == i,]
#   jpeg(paste('cluster_india_northeast_', i , '.jpg',sep=""),3000, 3000, res=300)
#   plot(map_india_northeast, main=paste("cluster: ",i))
#   plot(data_india_2014_northeast_violent_loopthis, add=T)
#   text(data_india_2014_northeast_violent_loopthis@coords, labels=paste(as.character(data_india_2014_northeast_violent_loopthis$event_start,format="%m/%d"), sep=","), cex= 0.7, pos=4)
#   dev.off()
# }

# ggplot(data1[as.numeric(data1$cluster_number)!=0,], aes(x=cluster_number)) + geom_histogram(binwidth = 1) + xlab("cluster number") + ylab("number of events")

# ggplot(data3_india_2014_northeast_with_cluster_info@data[as.numeric(data3_india_2014_northeast_with_cluster_info$cluster_spatiotemporal_infrastructure)!=0,], aes(x=cluster_spatiotemporal_infrastructure)) + geom_histogram(binwidth = 1) + xlab("cluster number") + ylab("number of events")

# > dbscan_northeast_0.13_4 <-  dbscan::dbscan(as.dist(distanceMatrix_india_2014_northeast), eps = 0.13, minPts = 4)
# large
# > data_india_2014_northeast_cluster1 <- data_india_2014_northeast[dbscan_northeast_0.13_4$cluster==1,]
# > data_india_2014_northeast_cluster14 <- data_india_2014_northeast[dbscan_northeast_0.13_4$cluster==14,]
# > data_india_2014_northeast_cluster24 <- data_india_2014_northeast[dbscan_northeast_0.13_4$cluster==24,]
# 
# small
# > data_india_2014_northeast_cluster2 <- data_india_2014_northeast[dbscan_northeast_0.13_4$cluster==2,]
# > data_india_2014_northeast_cluster3 <- data_india_2014_northeast[dbscan_northeast_0.13_4$cluster==3,]
# > data_india_2014_northeast_cluster4 <- data_india_2014_northeast[dbscan_northeast_0.13_4$cluster==4,]

n <- max(dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max11$cluster) + 1
m_spatiotemporal_max11 <- matrix( 0, ncol = 8, nrow = n)
colnames(m_spatiotemporal_max11) <- c("cluster", "Min.","1st Qu.","Median","Mean","3rd Qu.","Max.","sd")
for(i in 0:(n-1)){
  distMatrix <- distanceFunction_using_vectors(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max11$cluster==i,], 100, 30, var_count = 0, spatialORtemporal='spatiotemporal')
  std <- round(sd(as.dist(distMatrix)),4)
  m_spatiotemporal_max11[i+1,] <- c(i, as.vector(summary(as.dist(distMatrix))), std)
  print(paste("cluster:", m_spatiotemporal_max11[i+1,]))
}

n <- max(dbscan_distanceMatrix_india_2014_northeast_combined_max11$cluster) + 1
m_combined_max11 <- matrix( 0, ncol = 8, nrow = n)
colnames(m_combined_max11) <- c("cluster", "Min.","1st Qu.","Median","Mean","3rd Qu.","Max.","sd")
for(i in 0:(n-1)){
  distMatrix <- distanceFunction_using_vectors(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_combined_max11$cluster==i,], 100, 30, var_count = 2, spatialORtemporal='both')
  std <- round(sd(as.dist(distMatrix)),4)
  m_combined_max11[i+1,] <- c(i, as.vector(summary(as.dist(distMatrix))),std)
  print(paste("cluster:", m_combined_max11[i+1,]))
}

n <- max(dbscan_distanceMatrix_india_2014_northeast_socioeconomic_max11$cluster) + 1
m_socioeconomic_max11 <- matrix( 0, ncol = 8, nrow = n)
colnames(m_socioeconomic_max11) <- c("cluster", "Min.","1st Qu.","Median","Mean","3rd Qu.","Max.","sd")
for(i in 1:(n-1)){
  distMatrix <- distanceFunction_using_vectors(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_socioeconomic_max11$cluster==i,], 100, 30, var_count = 2, spatialORtemporal='socioeconomic')
  std <- round(sd(as.dist(distMatrix)),4)
  m_socioeconomic_max11[i+1,] <- c(i, as.vector(summary(as.dist(distMatrix))), std)
  print(paste("cluster:",i, m_socioeconomic_max11[i+1,]))
}

# > data2_india_2014_northeast_clusterresults_spatiotemporal <- data2_india_2014_northeast
# > data2_india_2014_northeast_clusterresults_spatiotemporal$cluster <-dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max11$cluster
# > data2_india_2014_northeast_clusterresults_socioeconomic <- data2_india_2014_northeast
# > data2_india_2014_northeast_clusterresults_socioeconomic$cluster <-dbscan_distanceMatrix_india_2014_northeast_socioeconomic_max11$cluster
# > data2_india_2014_northeast_clusterresults_combined <- data2_india_2014_northeast
# > data2_india_2014_northeast_clusterresults_combined$cluster <-dbscan_distanceMatrix_india_2014_northeast_combined_max11$cluster

# plot the map
plot(map_india_northeast_d, main="Northeastern India\nCluster No. 5", xlab="socioeconomic clustering, 5 events, eps: 0.099058, minPts: 4", lty=3)
plot(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_socioeconomic_max11$cluster==5,], col=3, add=T)

# pch=16 is solid circles
# plot(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster==4,], col=2, add=T, cex=0.5)

# > mean(data2_india_2014_northeast_clusterresults_combined[data2_india_2014_northeast_clusterresults_combined$cluster==0,]@data$literacyRate)
# [1] 0.5120787
# > sd(data2_india_2014_northeast_clusterresults_combined[data2_india_2014_northeast_clusterresults_combined$cluster==0,]@data$literacyRate)
# [1] 0.2080193

0.090052
n <- max(dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster) + 1
m_spatiotemporal_max10 <- matrix( 0, ncol = 8, nrow = n)
colnames(m_spatiotemporal_max10) <- c("cluster", "Min.","1st Qu.","Median","Mean","3rd Qu.","Max.","sd")
for(i in 0:(n-1)){
  distMatrix <- distanceFunction_using_vectors(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster==i,], 100, 30, var_count = 2, spatialORtemporal='spatiotemporal')
  std <- round(sd(as.dist(distMatrix)),4)
  m_spatiotemporal_max10[i+1,] <- c(i, as.vector(summary(as.dist(distMatrix))), std)
  print(m_spatiotemporal_max10[i+1,])
}


n <- max(dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster) + 1
m_combined_max10 <- matrix( 0, ncol = 8, nrow = n)
colnames(m_combined_max10) <- c("cluster", "Min.","1st Qu.","Median","Mean","3rd Qu.","Max.","sd")
for(i in 0:(n-1)){
  distMatrix <- distanceFunction_using_vectors(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster==i,], 100, 30, var_count = 2, spatialORtemporal='both')
  std <- round(sd(as.dist(distMatrix)),4)
  m_combined_max10[i+1,] <- c(i, as.vector(summary(as.dist(distMatrix))), std)
  print(m_combined_max10[i+1,])
}


n <- max(dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster) + 1
m_spatiotemporal_max10 <- matrix( 0, ncol = 8, nrow = n)
colnames(m_spatiotemporal_max10) <- c("cluster", "Min.","1st Qu.","Median","Mean","3rd Qu.","Max.","sd")
for(i in 1:(n-1)){
  print(i)
  distMatrix <- distanceFunction_using_vectors(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster==i,], 100, 30, var_count = 2, spatialORtemporal='spatiotemporal')
  std <- round(sd(as.dist(distMatrix)),4)
  m_spatiotemporal_max10[i+1,] <- c(i, as.vector(summary(as.dist(distMatrix))), std)
  print( m_spatiotemporal_max10[i+1,])
}
for(i in 57:(n-1)){
  print(i)
  distMatrix <- distanceFunction_using_vectors(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster==i,], 100, 30, var_count = 2, spatialORtemporal='spatiotemporal')
  std <- round(sd(as.dist(distMatrix)),4)
  m_spatiotemporal_max10[i+1,] <- c(i, as.vector(summary(as.dist(distMatrix))), std)
  print( m_spatiotemporal_max10[i+1,])
}


# TO CALCULATE the spatial distances between objects
m_spatial_spatiotemporal_max10 <- matrix( 0, ncol = 5, nrow = 7)
# 7 rows for the 3-smallest + 3-largest and 1-noise
colnames(m_spatial_spatiotemporal_max10) <- c("cluster", "Min.","Average","Max","Standard_deviation")
j <- 1
for(i in c(0,14,51,9,61,63,69)){
  distMatrix <- distanceFunction_spatial(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster==i,], dmax=100, tmax=30, var_count = 0)
  minimu <- round(min(as.dist(distMatrix)),4)
  avg <- round(mean(as.dist(distMatrix)),4)
  maxx <- round(max(as.dist(distMatrix)),4)
  std <- round(sd(as.dist(distMatrix)),4)
  m_spatial_spatiotemporal_max10[j,] <- c(i, minimu, avg, maxx, std)
  print( m_spatial_spatiotemporal_max10[j,])
  j <- j+1
}



## combined: centroids for each cluster
n <- max(dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster) + 1
dataframe_combined_max10 <- NA
dataframe_combined_max10 <- data.frame(1, 2.2, 2.2, 344, as.Date("2012/12/12",format="%Y/%m/%d"),as.Date("2012/12/12",format="%Y/%m/%d"), "cat" , 0.4, 0.5 )
colnames(dataframe_combined_max10) <- c("cluster", "x", "y", "p","event_start","event_end", "event_category", "literacyRate", "workerPop_main")

for(i in 0:(n-1)){
  xy <- as.vector(gCentroid(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster==i,])@coords)
  p <- mean(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster==i,]$p)
  event_date <- mean(data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster==i,]$event_start)
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
  dataframe_combined_max10[i+1,"event_category"] <- "cat"  
}
centroid_combined_max10 <- dataframe_combined_max10
coordinates(centroid_combined_max10)=~x+y
crs(centroid_combined_max10)="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
print(centroid_combined_max10@data)

# now calculating the intra cluster distances
n <- max(dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster) + 1
m_intracluster_combined <- matrix(0, ncol=3, nrow=1770)
colnames(m_intracluster_combined) <- c("cluster", "uniqueid", "distance")
k <- 1
for (i in 0:(n-1)){
  
  data_cluster_i <- data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster==i,]
  c_rows <- nrow(data_cluster_i)
  
  cc <- centroid_combined_max10[centroid_combined_max10$cluster==i,]
  event1 <- prepareArguments_forspecificuse(cc@data, 1, var_count=2)
  print(event1)
  points1 <- .pointsToMatrix(cc)
  var_count <- 2
  
  for (j in 1:c_rows){
    event2 <- prepareArguments(data_cluster_i@data, j)
    points2 <- .pointsToMatrix(data_cluster_i[j,])
    distance_event1_event2 <- distanceFunction_vectors(event1, points1, event2, points2, dmax=100, tmax=30, spatialORtemporal='both')
    m_intracluster_combined[k,1] <- i
    m_intracluster_combined[k,2] <- data_cluster_i@data[j,"uniqueid"]
    m_intracluster_combined[k,3] <- distance_event1_event2
    k <- k+1
  }
}

# lets import the roads too.
'/Users/sudbasnet/Documents/DataSources_SURGE/Infrastructure Data Final/SURGE-Infrastructure_India/gis_osm_roads_primary_secondary.shp'
roads_northeast <- shapefile("./shapefiles/roads_northeast.shp")
poi_india <- shapefile("./shapefiles/gis.osm_pois_free_1.shp")
poi_northeast <- poi_india[!is.na(sp::over(poi_india, map_india_northeast)[,1]),]
poi_northeast <- head(poi_northeast[poi_northeast@data$fclass %in% c('post_office','police','school','college','hospital','university','post_box'),])
poi_northeast_school <- poi_northeast[poi_northeast@data$fclass=='school',]
poi_northeast_college <- poi_northeast[poi_northeast@data$fclass=='college',]
poi_northeast_university <- poi_northeast[poi_northeast@data$fclass=='university',]
poi_northeast_police <- poi_northeast[poi_northeast@data$fclass=='police',]
poi_northeast_hospital <- poi_northeast[poi_northeast@data$fclass=='hospital',]
poi_northeast_postal <- poi_northeast[poi_northeast@data$fclass %in% c('post_box','post_box'),]

#to write a specific cluster links
write.xlsx(data3_india_2014_northeast@data[dbscan_distanceMatrix_india_2014_northeast_combined_infra_max10$cluster==90,c(1,2,4,5)], "data_northeast_spatiotemp_socio_infra_cluster_90.xlsx")
