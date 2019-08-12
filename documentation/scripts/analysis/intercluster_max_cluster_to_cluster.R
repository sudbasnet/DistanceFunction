n <- max(dbscan_distanceMatrix_india_2014_northeast_socioeconomic_max10$cluster)
m_intercluster_max_points_socioeconomic <- matrix(0, ncol=3, nrow=0)
colnames(m_intercluster_max_points_socioeconomic) <- c("cluster1","cluster2","distance")
k <- 1
for (i in 0:(n-1)){
    data_cluster_i <- m_intercluster_socioeconomic_onetoone[m_intercluster_socioeconomic_onetoone[,"cluster_from"]==i,]
    for (j in (i+1):n){
        data_cluster_j <- data_cluster_i[data_cluster_i[,"cluster_to"]==j,]
        m_dummy <- matrix(0, ncol=3, nrow=1)
        colnames(m_dummy) <- c("cluster1","cluster2","distance")
        m_intercluster_max_points_socioeconomic <- rbind(m_intercluster_max_points_socioeconomic, m_dummy)
        maxximum <- round(max(data_cluster_j[,5]),4)
        if (maxximum <0){stop(paste("i",i, "j", j, "max", maxximum))}
        m_intercluster_max_points_socioeconomic[k,1] <- i
        m_intercluster_max_points_socioeconomic[k,2] <- j
        m_intercluster_max_points_socioeconomic[k,3] <- maxximum
        k <- k+1
    }
}

n <- max(dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster)
m_intercluster_max_points_spatiotemporal <- matrix(0, ncol=3, nrow=0)
colnames(m_intercluster_max_points_spatiotemporal) <- c("cluster1","cluster2","distance")
k <- 1
for (i in 0:(n-1)){
    data_cluster_i <- m_intercluster_spatiotemporal_onetoone[m_intercluster_spatiotemporal_onetoone[,"cluster_from"]==i,]
    for (j in (i+1):n){
        data_cluster_j <- data_cluster_i[data_cluster_i[,"cluster_to"]==j,]
        m_dummy <- matrix(0, ncol=3, nrow=1)
        colnames(m_dummy) <- c("cluster1","cluster2","distance")
        m_intercluster_max_points_spatiotemporal <- rbind(m_intercluster_max_points_spatiotemporal, m_dummy)
        maxximum <- round(max(data_cluster_j[,5]),4)
        if (maxximum <0){stop(paste("i",i, "j", j, "max", maxximum))}
        m_intercluster_max_points_spatiotemporal[k,1] <- i
        m_intercluster_max_points_spatiotemporal[k,2] <- j
        m_intercluster_max_points_spatiotemporal[k,3] <- maxximum
        k <- k+1
    }
}

n <- max(dbscan_distanceMatrix_india_2014_northeast_combined_max10$cluster)
m_intercluster_max_points_combined <- matrix(0, ncol=3, nrow=0)
colnames(m_intercluster_max_points_combined) <- c("cluster1","cluster2","distance")
k <- 1
for (i in 0:(n-1)){
    data_cluster_i <- m_intercluster_combined_onetoone[m_intercluster_combined_onetoone[,"cluster_from"]==i,]
    for (j in (i+1):n){
        data_cluster_j <- data_cluster_i[data_cluster_i[,"cluster_to"]==j,]
        m_dummy <- matrix(0, ncol=3, nrow=1)
        colnames(m_dummy) <- c("cluster1","cluster2","distance")
        m_intercluster_max_points_combined <- rbind(m_intercluster_max_points_combined, m_dummy)
        maxximum <- round(max(data_cluster_j[,5]),4)
        if (maxximum <0){stop(paste("i",i, "j", j, "max", maxximum))}
        m_intercluster_max_points_combined[k,1] <- i
        m_intercluster_max_points_combined[k,2] <- j
        m_intercluster_max_points_combined[k,3] <- maxximum
        k <- k+1
    }
}


> round(mean(m_intercluster_max_points_spatiotemporal["cluster1"!=0, 3]),4)
[1] 0.9797
> round(mean(m_intercluster_max_points_socioeconomic["cluster1"!=0, 3]),4)
[1] 0.5151
> round(mean(m_intercluster_max_points_combined["cluster1"!=0, 3]),4)
[1] 0.6122