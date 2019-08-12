distanceFunction_spatial_actual <- function(eventData, fun=distHaversine) {
    points <- .pointsToMatrix(eventData)
    n = nrow(points)
    distanceMatrix_spatial_actual <- matrix(0, ncol=n, nrow=n)
    if (n == 1) {	
        return(distanceMatrix_spatial_actual) 	
    }
    for (i in 2:n) {
        for (j in 1:(i-1)){
            dspatial <- fun(points[i,], points[j,]) # default function is Haversine
            dspatial <- round(dspatial/1000 , 4) # rounding off to 4 decimal digits after turning meters to km
            # print(paste("dspatial > dmax", dspatial ,dmax))
            distanceMatrix_spatial_actual[i,j] = dspatial
        }
    }
    return(distanceMatrix_spatial_actual)
}

# THEN DO THIS FOR EACH KIND OF WEIGHT (SPATIO AND COMB)
clusters <- c(0,10,14,16,3,8,29)
m_cluster_summaries <- matrix( 0, ncol = 11, nrow = length(clusters))
# 7 rows for the 3-smallest + 3-largest and 1-noise
colnames(m_cluster_summaries) <- c("Cluster", "Events", "Min.","Average","Max","Standard_deviation", "Min(date)", "Max(date)", "Min.(km)", "Avg.(km)", "Max.(km)")
j <- 1
for(i in clusters){
    event_count <- nrow(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_infrastructure_max10$cluster==i,])
    distMatrix_temp <- distanceFunction_using_vectors_infra(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_infrastructure_max10$cluster==i,], dmax=100, tmax=30, var_count = 2, infra_count = 6, fun=distHaversine, weight_spatiotemporal =0.333333, weight_socioeconomic=0.333333, spatialORsocioORinfra = 'spatiotemporal_infrastructure')
    minm <- round(min(as.dist(distMatrix_temp)),4)
    avg <- round(mean(as.dist(distMatrix_temp)),4)
    maxx <- round(max(as.dist(distMatrix_temp)),4)
    std <- round(sd(as.dist(distMatrix_temp)),4)
    minm_date <- as.character(min(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_infrastructure_max10$cluster==i,]$event_start), format="%m/%d%/%Y")
    maxx_date <- as.character(max(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_infrastructure_max10$cluster==i,]$event_start), format="%m/%d%/%Y")
    distances_km <- distanceFunction_spatial_actual(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_infrastructure_max10$cluster==i,])
    minm_km <- min(as.dist(distances_km))
    avg_km <- round(mean(as.dist(distances_km)),4)
    maxx_km <- max(as.dist(distances_km))
    m_cluster_summaries[j,] <- c(i, event_count, minm, avg, maxx, std, minm_date, maxx_date, minm_km, avg_km, maxx_km)
    print( m_cluster_summaries[j,])
    j <- j+1
}

clusters <- c(0,4,25,12,13,62,3,6,15, 16,17,19)
m_cluster_summaries_all <- matrix( 0, ncol = 11, nrow = length(clusters))
# 7 rows for the 3-smallest + 3-largest and 1-noise
colnames(m_cluster_summaries_all) <- c("Cluster", "Events", "Min.","Average","Max","Standard_deviation", "Min(date)", "Max(date)", "Min.(km)", "Avg.(km)", "Max.(km)")
j <- 1
for(i in clusters){
    event_count <- nrow(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==i,])
    distMatrix_temp <- distanceFunction_using_vectors_infra(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==i,], dmax=100, tmax=30, var_count = 2, infra_count = 6, fun=distHaversine, weight_spatiotemporal =0.333333, weight_socioeconomic=0.333333, spatialORsocioORinfra = 'all')
    minm <- round(min(as.dist(distMatrix_temp)),4)
    avg <- round(mean(as.dist(distMatrix_temp)),4)
    maxx <- round(max(as.dist(distMatrix_temp)),4)
    std <- round(sd(as.dist(distMatrix_temp)),4)
    minm_date <- as.character(min(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==i,]$event_start), format="%m/%d%/%Y")
    maxx_date <- as.character(max(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==i,]$event_start), format="%m/%d%/%Y")
    distances_km <- distanceFunction_spatial_actual(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==i,])
    minm_km <- min(as.dist(distances_km))
    avg_km <- round(mean(as.dist(distances_km)),4)
    maxx_km <- max(as.dist(distances_km))
    m_cluster_summaries_all[j,] <- c(i, event_count, minm, avg, maxx, std, minm_date, maxx_date, minm_km, avg_km, maxx_km)
    print( m_cluster_summaries_all[j,])
    j <- j+1
}