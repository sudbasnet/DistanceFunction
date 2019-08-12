##get_centroid

n <- max(dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster)
m_intercluster_spatiotemporal <- matrix(0, ncol=3, nrow=0)
colnames(m_intercluster_spatiotemporal) <- c("cluster", "uniqueid", "distance")
k <- 1
for (i in 0:n){
    data_cluster_i <- data3_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster != i,]
    c_rows <- nrow(data_cluster_i)
    
    m_dummy <- matrix(0, ncol=3, nrow=c_rows)
    colnames(m_dummy) <- c("cluster", "uniqueid", "distance")
    m_intercluster_spatiotemporal <- rbind(m_intercluster_spatiotemporal,m_dummy)
    
    cc <- get_centroid(data3_india_2014_northeast, dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10, i)
    event1 <- prepareArguments(cc@data, 1, 2, 6)
    print(event1)
    points1 <- .pointsToMatrix(cc)
    var_count <- 2
    
    for (j in 1:c_rows){
        event2 <- prepareArguments(data_cluster_i@data, j, 2, 6)
        points2 <- .pointsToMatrix(data_cluster_i[j,])
        distance_event1_event2 <- distanceFunction_vectors(event1, points1, event2, points2, dmax=100, tmax=30, spatialORtemporal='spatiotemporal')
        m_intercluster_spatiotemporal[k,1] <- i
        m_intercluster_spatiotemporal[k,2] <- data_cluster_i@data[j,"uniqueid"]
        m_intercluster_spatiotemporal[k,3] <- distance_event1_event2
        k <- k+1
    }
}


n <- max(dbscan_distanceMatrix_india_2014_northeast_socioeconomic_max10$cluster) + 1
m_intercluster_socioeconomic <- matrix(0, ncol=3, nrow=0)
colnames(m_intercluster_socioeconomic) <- c("cluster", "uniqueid", "distance")
k <- 1
for (i in 0:(n-1)){
    
    data_cluster_i <- data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_socioeconomic_max10$cluster != i,]
    c_rows <- nrow(data_cluster_i)
    
    m_dummy <- matrix(0, ncol=3, nrow=c_rows)
    colnames(m_dummy) <- c("cluster", "uniqueid", "distance")
    m_intercluster_socioeconomic <- rbind(m_intercluster_socioeconomic,m_dummy)
    
    cc <- centroid_socioeconomic_max10[centroid_socioeconomic_max10$cluster==i,]
    event1 <- prepareArguments_forspecificuse(cc@data, 1, var_count=2)
    print(event1)
    points1 <- .pointsToMatrix(cc)
    var_count <- 2
    
    for (j in 1:c_rows){
        event2 <- prepareArguments(data_cluster_i@data, j)
        points2 <- .pointsToMatrix(data_cluster_i[j,])
        distance_event1_event2 <- distanceFunction_vectors(event1, points1, event2, points2, dmax=100, tmax=30, spatialORtemporal='socioeconomic')
        m_intercluster_socioeconomic[k,1] <- i
        m_intercluster_socioeconomic[k,2] <- data_cluster_i@data[j,"uniqueid"]
        m_intercluster_socioeconomic[k,3] <- distance_event1_event2
        k <- k+1
    }
}


n <- max(dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster) + 1
m_intercluster_combined <- matrix(0, ncol=3, nrow=0)
colnames(m_intercluster_combined) <- c("cluster", "uniqueid", "distance")
k <- 1
for (i in 0:(n-1)){
    
    data_cluster_i <- data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster!=i,]
    c_rows <- nrow(data_cluster_i)
    
    m_dummy <- matrix(0, ncol=3, nrow=c_rows)
    colnames(m_dummy) <- c("cluster", "uniqueid", "distance")
    m_intercluster_combined <- rbind(m_intercluster_combined,m_dummy)
    
    cc <- centroid_combined_max10[centroid_combined_max10$cluster==i,]
    event1 <- prepareArguments_forspecificuse(cc@data, 1, var_count=2)
    print(event1)
    points1 <- .pointsToMatrix(cc)
    var_count <- 2
    
    for (j in 1:c_rows){
        event2 <- prepareArguments(data_cluster_i@data, j)
        points2 <- .pointsToMatrix(data_cluster_i[j,])
        distance_event1_event2 <- distanceFunction_vectors(event1, points1, event2, points2, dmax=100, tmax=30, spatialORtemporal='both')
        m_intercluster_combined[k,1] <- i
        m_intercluster_combined[k,2] <- data_cluster_i@data[j,"uniqueid"]
        m_intercluster_combined[k,3] <- distance_event1_event2
        k <- k+1
    }
}

round(mean(m_intercluster_combined[m_intercluster_combined[,1]!=0,3]),4)
round(mean(m_intercluster_socioeconomic[m_intercluster_socioeconomic[,1]!=0,3]),4)
round(mean(m_intercluster_spatiotemporal[m_intercluster_spatiotemporal[,1]!=0,3]),4)
round(mean(m_intercluster_socioeconomic_onetoone[m_intercluster_socioeconomic_onetoone[,1]!=0,5]),4)
round(mean(m_intercluster_spatiotemporal_onetoone[m_intercluster_spatiotemporal_onetoone[,1]!=0,5]),4)
round(mean(m_intercluster_combined_onetoone[m_intercluster_combined_onetoone[,1]!=0,5]),4)
