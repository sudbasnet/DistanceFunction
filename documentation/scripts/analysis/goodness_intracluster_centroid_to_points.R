goodness_intracluster_centroid_to_points <- function(x, dbscan_data, distance_type){
    ## x is the dataframe
    ## distance_type can be "socioeconomic", "spatiotemporal", "infrastructure", "spatio_socio", "spatio_infra" or "all"
    n <- max(dbscan_data$cluster)
    m_intracluster_centroid_to_points <- matrix(0, ncol=3, nrow = 0)
    colnames(m_intracluster_centroid_to_points) <- c("cluster", "uniqueid", "distance")
    k <- 1
    for(i in 0:(n)){
        cluster_data <- x[dbscan_data$cluster==i,]
        size <- nrow(cluster_data)
        m_intracluster_centroid_to_points <- rbind(m_intracluster_centroid_to_points, matrix(0, ncol=3, nrow = size))
        
        cluster_centroid <- data.frame(1, 2.2, 2.2, 344 ,as.Date("2012/12/12",format="%Y/%m/%d") ,as.Date("2012/12/12",format="%Y/%m/%d"), "cat", 0.4, 0.5, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
        colnames(cluster_centroid) <- c("cluster", "x", "y", "p","event_start","event_end", "eventCategory", "literacyRate","workerPop_main","police_proximity", "postal_proximity", "hospital_proximity", "university_proximity", "college_proximity", "school_proximity" ,"police_density", "postal_density", "hospital_density", "university_density", "college_density" ,"school_density" )
        
        xy <- as.vector(gCentroid(x[dbscan_data$cluster==i,])@coords)
        p <- mean(x[dbscan_data$cluster==i,]$p)
        event_date <- mean(x[dbscan_data$cluster==i,]$event_start)
        literacyRate <- mean(x[dbscan_data$cluster==i,]$literacyRate)
        workerPop_main <- mean(x[dbscan_data$cluster==i,]$workerPop_main)
        
        cluster_centroid[1,"cluster"] <- i
        cluster_centroid[1,"x"] <- xy[1]
        cluster_centroid[1,"y"] <- xy[2]
        cluster_centroid[1,"p"] <- p
        cluster_centroid[1,"literacyRate"] <- literacyRate
        cluster_centroid[1,"workerPop_main"] <- workerPop_main
        cluster_centroid[1,"event_start"] <- event_date
        cluster_centroid[1,"event_end"] <- event_date
        cluster_centroid[1,"eventCategory"] <- "cat"
        
        cluster_centroid[1,"police_proximity"] <- mean(x[dbscan_data$cluster==i,]$police_proximity)
        cluster_centroid[1,"postal_proximity"] <- mean(x[dbscan_data$cluster==i,]$postal_proximity)
        cluster_centroid[1,"hospital_proximity"] <- mean(x[dbscan_data$cluster==i,]$hospital_proximity)
        cluster_centroid[1,"university_proximity"] <- mean(x[dbscan_data$cluster==i,]$university_proximity)
        cluster_centroid[1,"college_proximity"] <- mean(x[dbscan_data$cluster==i,]$college_proximity)
        cluster_centroid[1,"school_proximity"] <- mean(x[dbscan_data$cluster==i,]$school_proximity)
        
        cluster_centroid[1,"police_density"] <- mean(x[dbscan_data$cluster==i,]$police_density)
        cluster_centroid[1,"postal_density"] <- mean(x[dbscan_data$cluster==i,]$postal_density)
        cluster_centroid[1,"hospital_density"] <- mean(x[dbscan_data$cluster==i,]$hospital_density)
        cluster_centroid[1,"university_density"] <- mean(x[dbscan_data$cluster==i,]$university_density)
        cluster_centroid[1,"college_density"] <- mean(x[dbscan_data$cluster==i,]$college_density)
        cluster_centroid[1,"school_density"] <- mean(x[dbscan_data$cluster==i,]$school_density)
        
        # print(cluster_centroid)
        
        coordinates(cluster_centroid)=~x+y
        crs(cluster_centroid)="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
        
        centroid_event <- prepareArguments(cluster_centroid@data, 1, 2, 6)
        centroid_point <- .pointsToMatrix(cluster_centroid[1,])
        # print(centroid_event)
        
        for(j in 1:size){
            event2 <- prepareArguments(cluster_data@data, j, 2, 6)
            points2 <- .pointsToMatrix(cluster_data[j,])
            
            distance_event1_event2 <- distanceFunction_vectors_infra(centroid_event, centroid_point, event2, points2, dmax=100, tmax=30,  var_count = 2 , infra_count = 6, spatial_dist_fun = distHaversine, weight_spatiotemporal = 0.33, spatialORsocioORinfra = 'all')
            dist_ <- 0
            if (distance_type == "all") {
                dist_ <- (distance_event1_event2[1] + distance_event1_event2[2] + distance_event1_event2[3])/3;
            }
            if (distance_type == "infrastructure") {
                dist_ <- distance_event1_event2[3];
            }
            if (distance_type == "spatio_socio") {
                dist_ <- (distance_event1_event2[1] + distance_event1_event2[2])/2;
            }
            if (distance_type == "spatio_infra") {
                dist_ <- (distance_event1_event2[1] + distance_event1_event2[3])/2;
            }
            m_intracluster_centroid_to_points[k,1] <- i
            m_intracluster_centroid_to_points[k,2] <- cluster_data[j,]$uniqueid
            m_intracluster_centroid_to_points[k,3] <- dist_
            
            k <- k + 1
        }
    }
    return(round(mean( m_intracluster_centroid_to_points[m_intracluster_centroid_to_points[,1] != 0, 3] ), 4))
}
