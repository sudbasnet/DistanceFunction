goodness_intercluster_onetoone <- function(x, dbscan_df, distance_type){
    n <- max(dbscan_df$cluster)
    m_intercluster_dist_onetoone <- matrix(0, ncol=5, nrow=0)
    colnames(m_intercluster_dist_onetoone) <- c("cluster_from", "cluster_to", "uniqueid_1", "uniqueid_2", "distance")
    k <- 1
    for (c in 0:(n-1)){
        data_cluster_j <- x
        data_cluster_j$cluster <- dbscan_df$cluster
        data_cluster_i <- x
        data_cluster_i$cluster <- dbscan_df$cluster
        ncoll <- ncol(data_cluster_i)
        
        data_cluster_j <- data_cluster_j[data_cluster_j$cluster > c,]
        data_cluster_i <- data_cluster_i[data_cluster_i$cluster == c,]
        n_rows <- nrow(data_cluster_i)
        c_rows <- nrow(data_cluster_j)
        
        m_dummy <- matrix(0, ncol=5, nrow= c_rows * n_rows)
        colnames(m_dummy) <- c("cluster_from", "cluster_to", "uniqueid_1", "uniqueid_2", "distance")
        m_intercluster_dist_onetoone <- rbind(m_intercluster_dist_onetoone,m_dummy)
        
        for (i in 1:n_rows)
        {
            event1 <- prepareArguments(data_cluster_i@data[,c(1:(ncoll-1))], i, 2, 6)
            points1 <- .pointsToMatrix(data_cluster_i[i,])
            
            for (j in 1:c_rows){
                event2 <- prepareArguments(data_cluster_j@data[,c(1:(ncoll-1))], j, 2, 6)
                points2 <- .pointsToMatrix(data_cluster_j[j,])
                distance_event1_event2 <- distanceFunction_vectors_infra(event1, points1, event2, points2, dmax=100, tmax=30,  var_count = 2 , infra_count = 6, spatial_dist_fun = distHaversine, weight_spatiotemporal = 0.33, spatialORsocioORinfra = 'all')
                if(distance_event1_event2 > 1){
                    print(event1)
                    print(event2)
                    stop("distance greater than 1")
                }
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
                m_intercluster_dist_onetoone[k,1] <- data_cluster_i@data[i,"cluster"]
                m_intercluster_dist_onetoone[k,2] <- data_cluster_j@data[j,"cluster"]
                m_intercluster_dist_onetoone[k,3] <- data_cluster_i@data[i,"uniqueid"]
                m_intercluster_dist_onetoone[k,4] <- data_cluster_j@data[j,"uniqueid"]
                m_intercluster_dist_onetoone[k,5] <- dist_
                k <- k+1
            }
        }
    }
    return(as.data.frame(m_intercluster_dist_onetoone))
}



n <- max(dbscan_distanceMatrix_india_2014_northeast_socioeconomic_max10$cluster)
m_intercluster_dist_onetoone <- matrix(0, ncol=5, nrow=0)
colnames(m_intercluster_dist_onetoone) <- c("cluster_from", "cluster_to", "uniqueid_1", "uniqueid_2", "distance")
k <- 1
var_count <- 2
for (c in 0:(n-1)){
    data_cluster_j <- data2_india_2014_northeast
    data_cluster_j$cluster <- dbscan_distanceMatrix_india_2014_northeast_socioeconomic_max10$cluster
    data_cluster_i <- data2_india_2014_northeast
    data_cluster_i$cluster <- dbscan_distanceMatrix_india_2014_northeast_socioeconomic_max10$cluster
    ncoll <- ncol(data_cluster_i)
    
    data_cluster_j <- data_cluster_j[data_cluster_j$cluster > c,]
    data_cluster_i <- data_cluster_i[data_cluster_i$cluster == c,]
    n_rows <- nrow(data_cluster_i)
    c_rows <- nrow(data_cluster_j)
    
    m_dummy <- matrix(0, ncol=5, nrow= c_rows * n_rows)
    colnames(m_dummy) <- c("cluster_from", "cluster_to", "uniqueid_1", "uniqueid_2", "distance")
    m_intercluster_dist_onetoone <- rbind(m_intercluster_dist_onetoone,m_dummy)
    
    for (i in 1:n_rows)
    {
        event1 <- prepareArguments(data_cluster_i@data[,c(1:(ncoll-1))], i)
        points1 <- .pointsToMatrix(data_cluster_i[i,])
        
        for (j in 1:c_rows){
            event2 <- prepareArguments(data_cluster_j@data[,c(1:(ncoll-1))], j)
            points2 <- .pointsToMatrix(data_cluster_j[j,])
            distance_event1_event2 <- distanceFunction_vectors(event1, points1, event2, points2, dmax=100, tmax=30, spatialORtemporal='socioeconomic')
            if(distance_event1_event2 > 1){
                print(event1)
                print(event2)
                stop("distance greater than 1")
            }
            m_intercluster_dist_onetoone[k,1] <- data_cluster_i@data[i,"cluster"]
            m_intercluster_dist_onetoone[k,2] <- data_cluster_j@data[j,"cluster"]
            m_intercluster_dist_onetoone[k,3] <- data_cluster_i@data[i,"uniqueid"]
            m_intercluster_dist_onetoone[k,4] <- data_cluster_j@data[j,"uniqueid"]
            m_intercluster_dist_onetoone[k,5] <- distance_event1_event2
            k <- k+1
        }
    }
}

n <- max(dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster)
m_intercluster_spatiotemporal_onetoone <- matrix(0, ncol=5, nrow=0)
colnames(m_intercluster_spatiotemporal_onetoone) <- c("cluster_from", "cluster_to", "uniqueid_1", "uniqueid_2", "distance")
k <- 1
var_count <- 2
for (c in 0:(n-1)){
    data_cluster_j <- data2_india_2014_northeast
    data_cluster_j$cluster <- dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster
    data_cluster_i <- data2_india_2014_northeast
    data_cluster_i$cluster <- dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster
    ncoll <- ncol(data_cluster_i)
    
    data_cluster_j <- data_cluster_j[data_cluster_j$cluster > c,]
    data_cluster_i <- data_cluster_i[data_cluster_i$cluster == c,]
    n_rows <- nrow(data_cluster_i)
    c_rows <- nrow(data_cluster_j)
    
    m_dummy <- matrix(0, ncol=5, nrow= c_rows * n_rows)
    colnames(m_dummy) <- c("cluster_from", "cluster_to", "uniqueid_1", "uniqueid_2", "distance")
    m_intercluster_spatiotemporal_onetoone <- rbind(m_intercluster_spatiotemporal_onetoone,m_dummy)
    
    for (i in 1:n_rows)
    {
        event1 <- prepareArguments(data_cluster_i@data[,c(1:(ncoll-1))], i)
        points1 <- .pointsToMatrix(data_cluster_i[i,])
        
        for (j in 1:c_rows){
            event2 <- prepareArguments(data_cluster_j@data[,c(1:(ncoll-1))], j)
            points2 <- .pointsToMatrix(data_cluster_j[j,])
            distance_event1_event2 <- distanceFunction_vectors(event1, points1, event2, points2, dmax=100, tmax=30, spatialORtemporal='spatiotemporal')
            if(distance_event1_event2 > 1){
                print(event1)
                print(event2)
                stop("distance greater than 1")
            }
            m_intercluster_spatiotemporal_onetoone[k,1] <- data_cluster_i@data[i,"cluster"]
            m_intercluster_spatiotemporal_onetoone[k,2] <- data_cluster_j@data[j,"cluster"]
            m_intercluster_spatiotemporal_onetoone[k,3] <- data_cluster_i@data[i,"uniqueid"]
            m_intercluster_spatiotemporal_onetoone[k,4] <- data_cluster_j@data[j,"uniqueid"]
            m_intercluster_spatiotemporal_onetoone[k,5] <- distance_event1_event2
            k <- k+1
        }
    }
}



n <- max(dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster)
m_intercluster_combined_onetoone <- matrix(0, ncol=5, nrow=0)
colnames(m_intercluster_combined_onetoone) <- c("cluster_from", "cluster_to", "uniqueid_1", "uniqueid_2", "distance")
k <- 1
var_count <- 2
for (c in 0:(n-1)){
    data_cluster_j <- data2_india_2014_northeast
    data_cluster_j$cluster <- dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster
    data_cluster_i <- data2_india_2014_northeast
    data_cluster_i$cluster <- dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster
    ncoll <- ncol(data_cluster_i)
    
    data_cluster_j <- data_cluster_j[data_cluster_j$cluster > c,]
    data_cluster_i <- data_cluster_i[data_cluster_i$cluster == c,]
    n_rows <- nrow(data_cluster_i)
    c_rows <- nrow(data_cluster_j)
    
    m_dummy <- matrix(0, ncol=5, nrow= c_rows * n_rows)
    colnames(m_dummy) <- c("cluster_from", "cluster_to", "uniqueid_1", "uniqueid_2", "distance")
    m_intercluster_combined_onetoone <- rbind(m_intercluster_combined_onetoone,m_dummy)
    
    for (i in 1:n_rows)
    {
        event1 <- prepareArguments(data_cluster_i@data[,c(1:(ncoll-1))], i)
        points1 <- .pointsToMatrix(data_cluster_i[i,])
        
        for (j in 1:c_rows){
            event2 <- prepareArguments(data_cluster_j@data[,c(1:(ncoll-1))], j)
            points2 <- .pointsToMatrix(data_cluster_j[j,])
            distance_event1_event2 <- distanceFunction_vectors(event1, points1, event2, points2, dmax=100, tmax=30, spatialORtemporal='both')
            if(distance_event1_event2 > 1){
                print(event1)
                print(event2)
                stop("distance greater than 1")
            }
            m_intercluster_combined_onetoone[k,1] <- data_cluster_i@data[i,"cluster"]
            m_intercluster_combined_onetoone[k,2] <- data_cluster_j@data[j,"cluster"]
            m_intercluster_combined_onetoone[k,3] <- data_cluster_i@data[i,"uniqueid"]
            m_intercluster_combined_onetoone[k,4] <- data_cluster_j@data[j,"uniqueid"]
            m_intercluster_combined_onetoone[k,5] <- distance_event1_event2
            k <- k+1
        }
    }
}
