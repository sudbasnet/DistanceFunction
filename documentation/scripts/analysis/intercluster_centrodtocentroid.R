n <- max(dbscan_distanceMatrix_india_2014_northeast_socioeconomic_max10$cluster)
m_intercluster_socioeconomic_centroidtocentroid <- matrix(0, ncol=3, nrow=0)
colnames(m_intercluster_socioeconomic_centroidtocentroid) <- c("cluster1", "cluster2", "distance")
k <- 1
var_count <- 2
for (i in 0:(n-1)){
    c1 <- centroid_socioeconomic_max10[centroid_socioeconomic_max10$cluster==i,]
    event1 <- prepareArguments_forspecificuse(c1@data, 1, var_count=2)
    points1 <- .pointsToMatrix(c1)
    for (j in (i+1):n){
        m_dummy <- matrix(0, ncol=3, nrow=1)
        colnames(m_dummy) <- c("cluster1","cluster2","distance")
        m_intercluster_socioeconomic_centroidtocentroid <- rbind(m_intercluster_socioeconomic_centroidtocentroid, m_dummy)
        
        c2 <- centroid_socioeconomic_max10[centroid_socioeconomic_max10$cluster==j,]
        event2 <- prepareArguments_forspecificuse(c2@data, 1, var_count=2)
        points2 <- .pointsToMatrix(c2)
        
        distance_event1_event2 <- distanceFunction_vectors(event1, points1, event2, points2, dmax=100, tmax=30, spatialORtemporal='socioeconomic')
        
        m_intercluster_socioeconomic_centroidtocentroid[k,1] <- i
        m_intercluster_socioeconomic_centroidtocentroid[k,2] <- j
        m_intercluster_socioeconomic_centroidtocentroid[k,3] <- distance_event1_event2
        
        k <- k+1
    }
}


n <- max(dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster)
m_intercluster_spatiotemporal_centroidtocentroid <- matrix(0, ncol=3, nrow=0)
colnames(m_intercluster_spatiotemporal_centroidtocentroid) <- c("cluster1", "cluster2", "distance")
k <- 1
var_count <- 2
for (i in 0:(n-1)){
    c1 <- centroid_spatiotemporal_max10[centroid_spatiotemporal_max10$cluster==i,]
    event1 <- prepareArguments_forspecificuse(c1@data, 1, var_count=2)
    points1 <- .pointsToMatrix(c1)
    for (j in (i+1):n){
        m_dummy <- matrix(0, ncol=3, nrow=1)
        colnames(m_dummy) <- c("cluster1","cluster2","distance")
        m_intercluster_spatiotemporal_centroidtocentroid <- rbind(m_intercluster_spatiotemporal_centroidtocentroid, m_dummy)
        
        c2 <- centroid_spatiotemporal_max10[centroid_spatiotemporal_max10$cluster==j,]
        event2 <- prepareArguments_forspecificuse(c2@data, 1, var_count=2)
        points2 <- .pointsToMatrix(c2)
        
        distance_event1_event2 <- distanceFunction_vectors(event1, points1, event2, points2, dmax=100, tmax=30, spatialORtemporal='spatiotemporal')
        
        m_intercluster_spatiotemporal_centroidtocentroid[k,1] <- i
        m_intercluster_spatiotemporal_centroidtocentroid[k,2] <- j
        m_intercluster_spatiotemporal_centroidtocentroid[k,3] <- distance_event1_event2
        
        k <- k+1
    }
}




n <- max(dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster)
m_intercluster_combined_centroidtocentroid <- matrix(0, ncol=3, nrow=0)
colnames(m_intercluster_combined_centroidtocentroid) <- c("cluster1", "cluster2", "distance")
k <- 1
var_count <- 2
for (i in 0:n){
    c1 <- centroid_combined_max10[centroid_combined_max10$cluster==i,]
    event1 <- prepareArguments_forspecificuse(c1@data, 1, var_count=2)
    points1 <- .pointsToMatrix(c1)
    for (j in (i+1):n){
        m_dummy <- matrix(0, ncol=3, nrow=1)
        colnames(m_dummy) <- c("cluster1","cluster2","distance")
        m_intercluster_combined_centroidtocentroid <- rbind(m_intercluster_combined_centroidtocentroid, m_dummy)
        
        c2 <- centroid_combined_max10[centroid_combined_max10$cluster==j,]
        event2 <- prepareArguments_forspecificuse(c2@data, 1, var_count=2)
        points2 <- .pointsToMatrix(c2)
        
        distance_event1_event2 <- distanceFunction_vectors(event1, points1, event2, points2, dmax=100, tmax=30, spatialORtemporal='both')
        
        m_intercluster_combined_centroidtocentroid[k,1] <- i
        m_intercluster_combined_centroidtocentroid[k,2] <- j
        m_intercluster_combined_centroidtocentroid[k,3] <- distance_event1_event2
        
        k <- k+1
    }
}



goodness_cluster_intercluster_centroid_to_centroid <- function(x, dbscan_, distance_type){
    n <- max(dbscan_$cluster)
    m_intercluster_centroidtocentroid <- matrix(0, ncol=3, nrow=0)
    colnames(m_intercluster_centroidtocentroid) <- c("cluster1", "cluster2", "distance")
    k <- 1
    for (i in 0:(n-1)){
        c1 <- get_centroid(x, dbscan_, i)
        event1 <- prepareArguments(c1@data, 1, 2, 6)
        points1 <- .pointsToMatrix(c1)
        for (j in (i+1):n){
            m_dummy <- matrix(0, ncol=3, nrow=1)
            colnames(m_dummy) <- c("cluster1","cluster2","distance")
            m_intercluster_centroidtocentroid <- rbind(m_intercluster_centroidtocentroid, m_dummy)
            c2 <- get_centroid(x, dbscan_, j)
            event2 <- prepareArguments(c2@data, 1, 2, 6)
            points2 <- .pointsToMatrix(c2)
            
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
            m_intercluster_centroidtocentroid[k,1] <- i
            m_intercluster_centroidtocentroid[k,2] <- j
            m_intercluster_centroidtocentroid[k,3] <- dist_
            
            k <- k+1
        }
    }
    return(round(mean(m_intercluster_centroidtocentroid[m_intercluster_centroidtocentroid[,1]!=0, 3]),4))
}
