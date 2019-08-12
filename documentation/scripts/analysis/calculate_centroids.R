get_centroid <- function(x, dbscan_, clusterNumber){
## combined
    n <- max(dbscan_$cluster) + 1
    cluster_centroid <- data.frame(1, 2.2, 2.2, 344 , as.Date("2012/12/12",format="%Y/%m/%d") ,as.Date("2012/12/12",format="%Y/%m/%d"),"category", 0.4, 0.5, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
    colnames(cluster_centroid) <- c("cluster", "x", "y", "p","event_start","event_end", "eventCategory", "literacyRate","workerPop_main", "police_proximity", "postal_proximity", "hospital_proximity", "university_proximity", "college_proximity", "school_proximity" ,"police_density", "postal_density", "hospital_density", "university_density", "college_density" ,"school_density" )
    xy <- as.vector(gCentroid(x[dbscan_$cluster==clusterNumber,])@coords)
    p <- mean(x[dbscan_$cluster==clusterNumber,]$p)
    event_date <- mean(x[dbscan_$cluster == clusterNumber,]$event_start)
    literacyRate <- mean(x[dbscan_$cluster == clusterNumber,]$literacyRate)
    workerPop_main <- mean(x[dbscan_$cluster == clusterNumber,]$workerPop_main)
    cluster_centroid[1,"cluster"] <- clusterNumber
    cluster_centroid[1,"x"] <- xy[1]
    cluster_centroid[1,"y"] <- xy[2]
    cluster_centroid[1,"p"] <- p
    cluster_centroid[1,"literacyRate"] <- literacyRate
    cluster_centroid[1,"workerPop_main"] <- workerPop_main
    cluster_centroid[1,"event_start"] <- event_date
    cluster_centroid[1,"event_end"] <- event_date
    cluster_centroid[1,"eventCategory"] <- "category"

    cluster_centroid[1,"police_proximity"] <- mean(x[dbscan_$cluster == clusterNumber,]$police_proximity)
    cluster_centroid[1,"postal_proximity"] <- mean(x[dbscan_$cluster == clusterNumber,]$postal_proximity)
    cluster_centroid[1,"hospital_proximity"] <- mean(x[dbscan_$cluster == clusterNumber,]$hospital_proximity)
    cluster_centroid[1,"university_proximity"] <- mean(x[dbscan_$cluster == clusterNumber,]$university_proximity)
    cluster_centroid[1,"college_proximity"] <- mean(x[dbscan_$cluster == clusterNumber,]$college_proximity)
    cluster_centroid[1,"school_proximity"] <- mean(x[dbscan_$cluster == clusterNumber,]$school_proximity)

    cluster_centroid[1,"police_density"] <- mean(x[dbscan_$cluster == clusterNumber,]$police_density)
    cluster_centroid[1,"postal_density"] <- mean(x[dbscan_$cluster == clusterNumber,]$postal_density)
    cluster_centroid[1,"hospital_density"] <- mean(x[dbscan_$cluster == clusterNumber,]$hospital_density)
    cluster_centroid[1,"university_density"] <- mean(x[dbscan_$cluster == clusterNumber,]$university_density)
    cluster_centroid[1,"college_density"] <- mean(x[dbscan_$cluster == clusterNumber,]$college_density)
    cluster_centroid[1,"school_density"] <- mean(x[dbscan_$cluster == clusterNumber,]$school_density)

    coordinates(cluster_centroid)=~x+y
    crs(cluster_centroid)="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    return(cluster_centroid)
}
