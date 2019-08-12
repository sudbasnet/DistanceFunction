goodness_intercluster_centroid_to_points <- function(x, dbscan_, distance_type){
    n <- max(dbscan_$cluster)
    m_intercluster_centroid_to_points <- matrix(0, ncol=3, nrow=0)
    colnames(m_intercluster_centroid_to_points) <- c("cluster", "uniqueid", "distance")
    k <- 1
    for (i in 0:n){
        data_cluster_i <- x[dbscan_$cluster != i,]
        c_rows <- nrow(data_cluster_i)
        
        m_dummy <- matrix(0, ncol=3, nrow=c_rows)
        colnames(m_dummy) <- c("cluster", "uniqueid", "distance")
        m_intercluster_centroid_to_points <- rbind(m_intercluster_centroid_to_points,m_dummy)
        
        cc <- get_centroid(x, dbscan_, i)
        event1 <- prepareArguments(cc@data, 1, 2, 6)
        points1 <- .pointsToMatrix(cc)
        var_count <- 2
        
        for (j in 1:c_rows){
            event2 <- prepareArguments(data_cluster_i@data, j, 2, 6)
            points2 <- .pointsToMatrix(data_cluster_i[j,])
            distance_event1_event2 <- distanceFunction_vectors_infra(event1, points1, event2, points2, dmax=100, tmax=30,  var_count = 2 , infra_count = 6, spatial_dist_fun = distHaversine, weight_spatiotemporal = 0.33, spatialORsocioORinfra = 'all')
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
            m_intercluster_centroid_to_points[k,1] <- i
            m_intercluster_centroid_to_points[k,2] <- data_cluster_i@data[j,"uniqueid"]
            m_intercluster_centroid_to_points[k,3] <- dist_
            k <- k+1
        }
    }
    return(round(mean(m_intercluster_centroid_to_points[m_intercluster_centroid_to_points[,1]!=0,3]),4))
}


