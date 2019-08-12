n <- max(dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster)
m_intracluster_spatiotemporal_onetoone <- matrix(0, ncol=4, nrow=2000)
colnames(m_intracluster_spatiotemporal_onetoone) <- c("cluster", "uniqueid_1", "uniqueid_2", "distance")
k <- 1
var_count <- 2
for (c in 0:(n-1)){
    data_cluster_i <- data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster == c,]
    rows <- nrow(data_cluster_i)

    for (i in 1:(rows-1)){
        event1 <- prepareArguments(data_cluster_i@data, i)
        points1 <- .pointsToMatrix(data_cluster_i[i,])
        for (j in (i+1):rows){
            event2 <- prepareArguments(data_cluster_i@data, j)
            points2 <- .pointsToMatrix(data_cluster_i[j,])
            # print("yo")
            distance_event1_event2 <- distanceFunction_vectors(event1, points1, event2, points2, dmax=100, tmax=30, spatialORtemporal='spatiotemporal')
            # print("tyo")

            m_intracluster_spatiotemporal_onetoone[k,1] <- c
            m_intracluster_spatiotemporal_onetoone[k,2] <- data_cluster_i@data[i,"uniqueid"]
            m_intracluster_spatiotemporal_onetoone[k,3] <- data_cluster_i@data[j,"uniqueid"]
            m_intracluster_spatiotemporal_onetoone[k,4] <- distance_event1_event2
        }
        k <- k + 1
    }
}

n <- max(dbscan_distanceMatrix_india_2014_northeast_socioeconomic_max10$cluster)
m_intracluster_socioeconomic_onetoone <- matrix(0, ncol=4, nrow=2000)
colnames(m_intracluster_socioeconomic_onetoone) <- c("cluster", "uniqueid_1", "uniqueid_2", "distance")
k <- 1
var_count <- 2
for (c in 1:(n-1)){
    data_cluster_i <- data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_socioeconomic_max10$cluster == c,]
    rows <- nrow(data_cluster_i)
    for (i in 1:(rows-1)){
        event1 <- prepareArguments(data_cluster_i@data, i)
        points1 <- .pointsToMatrix(data_cluster_i[i,])
        for (j in (i+1):rows){
            event2 <- prepareArguments(data_cluster_i@data, j)
            points2 <- .pointsToMatrix(data_cluster_i[j,])
            # print("yo")
            distance_event1_event2 <- distanceFunction_vectors(event1, points1, event2, points2, dmax=100, tmax=30, spatialORtemporal='socioeconomic')
            # print("tyo")

            m_intracluster_socioeconomic_onetoone[k,1] <- c
            m_intracluster_socioeconomic_onetoone[k,2] <- data_cluster_i@data[i,"uniqueid"]
            m_intracluster_socioeconomic_onetoone[k,3] <- data_cluster_i@data[j,"uniqueid"]
            m_intracluster_socioeconomic_onetoone[k,4] <- distance_event1_event2
        }
        k <- k + 1
    }
}

n <- max(dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster)
m_intracluster_combined_onetoone <- matrix(0, ncol=4, nrow=2000)
colnames(m_intracluster_combined_onetoone) <- c("cluster", "uniqueid_1", "uniqueid_2", "distance")
k <- 1
var_count <- 2
for (c in 0:(n-1)){
    data_cluster_i <- data2_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster == c,]
    rows <- nrow(data_cluster_i)

    for (i in 1:(rows-1)){
        event1 <- prepareArguments(data_cluster_i@data, i)
        points1 <- .pointsToMatrix(data_cluster_i[i,])
        for (j in (i+1):rows){
            event2 <- prepareArguments(data_cluster_i@data, j)
            points2 <- .pointsToMatrix(data_cluster_i[j,])
            # print("yo")
            distance_event1_event2 <- distanceFunction_vectors(event1, points1, event2, points2, dmax=100, tmax=30, spatialORtemporal='both')
            # print("tyo")

            m_intracluster_combined_onetoone[k,1] <- c
            m_intracluster_combined_onetoone[k,2] <- data_cluster_i@data[i,"uniqueid"]
            m_intracluster_combined_onetoone[k,3] <- data_cluster_i@data[j,"uniqueid"]
            m_intracluster_combined_onetoone[k,4] <- distance_event1_event2
        }
        k <- k + 1
    }
}

# cleaning them up
m_intracluster_spatiotemporal_onetoone <- m_intracluster_spatiotemporal_onetoone[m_intracluster_spatiotemporal_onetoone[,3]!= 0,]
m_intracluster_socioeconomic_onetoone <- m_intracluster_socioeconomic_onetoone[m_intracluster_socioeconomic_onetoone[,3]!= 0,]
m_intracluster_combined_onetoone <- m_intracluster_combined_onetoone[m_intracluster_combined_onetoone[,3]!= 0,]

# mean values:
round(mean(m_intracluster_spatiotemporal_onetoone[m_intracluster_spatiotemporal_onetoone[,1]!=0,4]),4)
round(mean(m_intracluster_socioeconomic_onetoone[m_intracluster_socioeconomic_onetoone[,1]!=0,4]),4)
round(mean(m_intracluster_combined_onetoone[m_intracluster_combined_onetoone[,1]!=0,4]),4)


n <- max(dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster)
m_intracluster_onetoone <- matrix(0, ncol=4, nrow=n*n)
colnames(m_intracluster_onetoone) <- c("cluster", "uniqueid_1", "uniqueid_2", "distance")
k <- 1
for (c in 0:(n-1)){
    data_cluster_i <- data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster == c,]
    rows <- nrow(data_cluster_i)
    
    for (i in 1:(rows-1)){
        event1 <- prepareArguments(data_cluster_i@data, i, 2, 6)
        points1 <- .pointsToMatrix(data_cluster_i[i,])
        for (j in (i+1):rows){
            event2 <- prepareArguments(data_cluster_i@data, j, 2, 6)
            points2 <- .pointsToMatrix(data_cluster_i[j,])
            # print("yo")
            distance_event1_event2 <- distanceFunction_vectors_infra(event1, points1, event2, points2, dmax=100, tmax=30,  var_count = 2 , infra_count = 6, spatial_dist_fun = distHaversine, weight_spatiotemporal = 0.33, spatialORsocioORinfra = 'all')
            distance_e1_e2 <- (distance_event1_event2[1] + distance_event1_event2[2] + distance_event1_event2[3])/3
            
            m_intracluster_onetoone[k,1] <- c
            m_intracluster_onetoone[k,2] <- data_cluster_i@data[i,"uniqueid"]
            m_intracluster_onetoone[k,3] <- data_cluster_i@data[j,"uniqueid"]
            m_intracluster_onetoone[k,4] <- distance_e1_e2
        }
        k <- k + 1
    }
}

n <- max(dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster)
m_intracluster_onetoone <- matrix(0, ncol=4, nrow=0)
colnames(m_intracluster_onetoone) <- c("cluster", "uniqueid_1", "uniqueid_2", "distance")
k <- 1
for (c in 0:(n-1)){
    data_cluster_i <- data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster == c,]
    rows <- nrow(data_cluster_i)
    
    for (i in 1:(rows-1)){
        m_intracluster_onetoone <- rbind(m_intracluster_onetoone, matrix(0, ncol=4, nrow=1))
        event1 <- prepareArguments(data_cluster_i@data, i, 2, 6)
        points1 <- .pointsToMatrix(data_cluster_i[i,])
        for (j in (i+1):rows){
            event2 <- prepareArguments(data_cluster_i@data, j, 2, 6)
            points2 <- .pointsToMatrix(data_cluster_i[j,])
            # print("yo")
            distance_event1_event2 <- distanceFunction_vectors_infra(event1, points1, event2, points2, dmax=100, tmax=30,  var_count = 2 , infra_count = 6, spatial_dist_fun = distHaversine, weight_spatiotemporal = 0.33, spatialORsocioORinfra = 'infrasturcture')
            distance_e1_e2 <- (distance_event1_event2[1] + distance_event1_event2[2] + distance_event1_event2[3])/3
            
            m_intracluster_onetoone[k,1] <- c
            m_intracluster_onetoone[k,2] <- data_cluster_i@data[i,"uniqueid"]
            m_intracluster_onetoone[k,3] <- data_cluster_i@data[j,"uniqueid"]
            m_intracluster_onetoone[k,4] <- distance_e1_e2
        }
        k <- k + 1
    }
}

n <- max(dbscan_india_2014_northeast_infrastructure_max10$cluster)
m_intracluster_infrastructure_onetoone <- matrix(0, ncol=4, nrow=1)
colnames(m_intracluster_infrastructure_onetoone) <- c("cluster", "uniqueid_1", "uniqueid_2", "distance")
k <- 1
for (c in 1:(n-1)){
    data_cluster_i <- data3_india_2014_northeast[dbscan_india_2014_northeast_infrastructure_max10$cluster == c,]
    rows <- nrow(data_cluster_i)
    
    for (i in 1:(rows-1)){
        event1 <- prepareArguments(data_cluster_i@data, i, 2, 6)
        points1 <- .pointsToMatrix(data_cluster_i[i,])
        for (j in (i+1):rows){
            event2 <- prepareArguments(data_cluster_i@data, j, 2, 6)
            points2 <- .pointsToMatrix(data_cluster_i[j,])
            # print("yo")
            distance_event1_event2 <- distanceFunction_vectors_infra(event1, points1, event2, points2, dmax=100, tmax=30,  var_count = 2 , infra_count = 6, spatial_dist_fun = distHaversine, weight_spatiotemporal = 0.33, spatialORsocioORinfra = 'all')
            distance_e1_e2 <- distance_event1_event2[3]
            
            m_intracluster_infrastructure_onetoone[k,1] <- c
            m_intracluster_infrastructure_onetoone[k,2] <- data_cluster_i@data[i,"uniqueid"]
            m_intracluster_infrastructure_onetoone[k,3] <- data_cluster_i@data[j,"uniqueid"]
            m_intracluster_infrastructure_onetoone[k,4] <- distance_e1_e2
        }
        k <- k + 1
        m_intracluster_infrastructure_onetoone <- rbind(m_intracluster_infrastructure_onetoone, matrix(0, ncol=4, nrow=1))
    }
}

goodness_intracluster_onetoone <- function(x, dbscan_, distance_type){
    n <- max(dbscan_$cluster)
    m_intracluster_onetoone <- matrix(0, ncol=4, nrow=0)
    colnames(m_intracluster_onetoone) <- c("cluster", "uniqueid_1", "uniqueid_2", "distance")
    k <- 1
    for (c in 1:n){
        ## ignore noise cluster '0'
        data_cluster_i <- x[dbscan_$cluster == c,]
        rows <- nrow(data_cluster_i)
        ## find the number of records in this cluster and caclulate distance between each other
        
        for (i in 1:(rows-1)){
            event1 <- prepareArguments(data_cluster_i@data, i, 2, 6)
            points1 <- .pointsToMatrix(data_cluster_i[i,])
            for (j in (i+1):rows){
                event2 <- prepareArguments(data_cluster_i@data, j, 2, 6)
                points2 <- .pointsToMatrix(data_cluster_i[j,])
                # print("yo")
                m_intracluster_onetoone <- rbind(m_intracluster_onetoone, matrix(0, ncol=4, nrow=1))
                distance_event1_event2 <- distanceFunction_vectors_infra(event1, points1, event2, points2, dmax=100, tmax=30,  var_count = 2 , infra_count = 6, spatial_dist_fun = distHaversine, weight_spatiotemporal = 0.33, spatialORsocioORinfra = 'all')
                dist_ <- 0
                if (distance_type == "all") {
                    dist_ <- (distance_event1_event2[1] + distance_event1_event2[2] + distance_event1_event2[3])/3;
                }
                if (distance_type == "infrastructure") {
                    dist_ <- distance_event1_event2[3];
                }
                if (distance_type == "socioeconomic") {
                    dist_ <- distance_event1_event2[2];
                }
                if (distance_type == "spatiotemporal") {
                    dist_ <- distance_event1_event2[1];
                }
                if (distance_type == "spatio_socio") {
                    dist_ <- (distance_event1_event2[1] + distance_event1_event2[2])/2;
                }
                if (distance_type == "spatio_infra") {
                    dist_ <- (distance_event1_event2[1] + distance_event1_event2[3])/2;
                }
                m_intracluster_onetoone[k,1] <- c
                m_intracluster_onetoone[k,2] <- data_cluster_i@data[i,"uniqueid"]
                m_intracluster_onetoone[k,3] <- data_cluster_i@data[j,"uniqueid"]
                m_intracluster_onetoone[k,4] <- dist_
                k <- k + 1
            }
        }
    }
    return(as.data.frame(m_intracluster_onetoone))
}
