clusters <- c(0, 14, 51, 9, 61, 63, 69)
m_cluster_summaries_spatiotemporal <- matrix( 0, ncol = 11, nrow = length(clusters))
# 7 rows for the 3-smallest + 3-largest and 1-noise
colnames(m_cluster_summaries_spatiotemporal) <- c("Cluster", "Events", "Min.","Average","Max","Standard_deviation", "Min(date)", "Max(date)", "Min.(km)", "Avg.(km)", "Max.(km)")
j <- 1
for(i in clusters){
    event_count <- nrow(data3_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster==i,])
    distMatrix_temp <- distanceFunction_using_vectors_infra(data3_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster==i,], dmax=100, tmax=30, var_count = 2, infra_count = 6, fun=distHaversine, weight_spatiotemporal =1, weight_socioeconomic=0, spatialORsocioORinfra = 'spatiotemporal')
    minm <- round(min(as.dist(distMatrix_temp)),4)
    avg <- round(mean(as.dist(distMatrix_temp)),4)
    maxx <- round(max(as.dist(distMatrix_temp)),4)
    std <- round(sd(as.dist(distMatrix_temp)),4)
    minm_date <- as.character(min(data3_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster==i,]$event_start), format="%m/%d%/%Y")
    maxx_date <- as.character(max(data3_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster==i,]$event_start), format="%m/%d%/%Y")
    distances_km <- distanceFunction_spatial_actual(data3_india_2014_northeast[dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10$cluster==i,])
    minm_km <- min(as.dist(distances_km))
    avg_km <- round(mean(as.dist(distances_km)),4)
    maxx_km <- max(as.dist(distances_km))
    m_cluster_summaries_spatiotemporal[j,] <- c(i, event_count, minm, avg, maxx, std, minm_date, maxx_date, minm_km, avg_km, maxx_km)
    ##    print( m_cluster_summaries_spatiotemporal_socioeconomic_infrastructure[j,])
    j <- j+1
}
m_cluster_summaries_spatiotemporal <- as.data.frame(m_cluster_summaries_spatiotemporal)

clusters <- c(0,4,25,12,13,62,92,94,96,99,100)
m_cluster_summaries_spatiotemporal_socioeconomic_infrastructure <- matrix( 0, ncol = 15, nrow = length(clusters))
# 7 rows for the 3-smallest + 3-largest and 1-noise
colnames(m_cluster_summaries_spatiotemporal_socioeconomic_infrastructure) <- c("Cluster", "Events", "Min.","Average","Max","Standard_deviation", "Min(date)", "Max(date)", "Min.(km)", "Avg.(km)",
"Max.(km)","Avg.(Literacy Rate)", "Std.Dev.(Literacy Rate)", "Avg.(Worker Population)", "Std.Dev.(Worker Population)")
j <- 1
for(i in clusters){
	event_count <- nrow(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==i,])
	distMatrix_temp <- distanceFunction_using_vectors_infra(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==i,], dmax=100, tmax=30, var_count = 2, infra_count = 6, fun=distHaversine, weight_spatiotemporal =0.333333, weight_socioeconomic=0.333333, spatialORsocioORinfra = 'all')
	minm <- round(min(as.dist(distMatrix_temp)),4)
	avg <- round(mean(as.dist(distMatrix_temp)),4)
	maxx <- round(max(as.dist(distMatrix_temp)),4)
	std <- round(sd(as.dist(distMatrix_temp)),4)
	minm_date <- as.character(mean(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==i,]$event_start), format="%m/%d%/%Y")
	maxx_date <- as.character(max(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==i,]$event_start), format="%m/%d%/%Y")
	distances_km <- distanceFunction_spatial_actual(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==i,])
	minm_km <- min(as.dist(distances_km))
	avg_km <- round(mean(as.dist(distances_km)),4)
	maxx_km <- max(as.dist(distances_km))
    avg_literacy_rate <- round(mean(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==i,]$literacyRate),4)
    std_literacy_rate <- round(std(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==i,]$literacyRate),4)
    avg_worker_pop <- round(mean(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==i,]$workerPop_main),4)
    std_worker_pop <- round(std(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==i,]$workerPop_main),4)
	m_cluster_summaries_spatiotemporal_socioeconomic_infrastructure[j,] <- c(i, event_count, minm, avg, maxx, std, minm_date, maxx_date, minm_km, avg_km, maxx_km, avg_literacy_rate, std_literacy_rate, avg_worker_pop, std_worker_pop)
	print( m_cluster_summaries_spatiotemporal_socioeconomic_infrastructure[j,])
	j <- j+1
	}
m_cluster_summaries_spatiotemporal_socioeconomic_infrastructure <- as.data.frame(m_cluster_summaries_spatiotemporal_socioeconomic_infrastructure)


clusters <- c(0,29,8,3,35,4,60,61,69,73,74)
m_cluster_summaries_spatiotemporal_infrastructure <- matrix( 0, ncol = 11, nrow = length(clusters))
# 7 rows for the 3-smallest + 3-largest and 1-noise
colnames(m_cluster_summaries_spatiotemporal_infrastructure) <- c("Cluster", "Events", "Min.","Average","Max","Standard_deviation", "Min(date)", "Max(date)", "Min.(km)", "Avg.(km)", "Max.(km)")
j <- 1
for(i in clusters){
	event_count <- nrow(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_infrastructure_max10$cluster==i,])
	distMatrix_temp <- distanceFunction_using_vectors_infra(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_infrastructure_max10$cluster==i,], dmax=100, tmax=30, var_count = 2, infra_count = 6, fun=distHaversine, weight_spatiotemporal =0.5, weight_socioeconomic=0, spatialORsocioORinfra = 'spatiotemporal_infrastructure')
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
	m_cluster_summaries_spatiotemporal_infrastructure[j,] <- c(i, event_count, minm, avg, maxx, std, minm_date, maxx_date, minm_km, avg_km, maxx_km)
	print( m_cluster_summaries_spatiotemporal_infrastructure[j,])
	j <- j+1
	}
m_cluster_summaries_spatiotemporal_infrastructure <- as.data.frame(m_cluster_summaries_spatiotemporal_infrastructure)


clusters <- c(1:8)
m_cluster_summaries_infrastructure <- matrix( 0, ncol = 11, nrow = length(clusters))
# 7 rows for the 3-smallest + 3-largest and 1-noise
colnames(m_cluster_summaries_infrastructure) <- c("Cluster", "Events", "Min.","Average","Max","Standard_deviation", "Min(date)", "Max(date)", "Min.(km)", "Avg.(km)", "Max.(km)")
j <- 1
for(i in clusters){
    event_count <- nrow(data3_india_2014_northeast[dbscan_india_2014_northeast_infrastructure_max10$cluster==i,])
    distMatrix_temp <- distanceFunction_using_vectors_infra(data3_india_2014_northeast[dbscan_india_2014_northeast_infrastructure_max10$cluster==i,], dmax=100, tmax=30, var_count = 2, infra_count = 6, fun=distHaversine, weight_spatiotemporal =1, weight_socioeconomic=0, spatialORsocioORinfra = 'spatiotemporal')
    minm <- round(min(as.dist(distMatrix_temp)),4)
    avg <- round(mean(as.dist(distMatrix_temp)),4)
    maxx <- round(max(as.dist(distMatrix_temp)),4)
    std <- round(sd(as.dist(distMatrix_temp)),4)
    minm_date <- as.character(min(data3_india_2014_northeast[dbscan_india_2014_northeast_infrastructure_max10$cluster==i,]$event_start), format="%m/%d%/%Y")
    maxx_date <- as.character(max(data3_india_2014_northeast[dbscan_india_2014_northeast_infrastructure_max10$cluster==i,]$event_start), format="%m/%d%/%Y")
    distances_km <- distanceFunction_spatial_actual(data3_india_2014_northeast[dbscan_india_2014_northeast_infrastructure_max10$cluster==i,])
    minm_km <- min(as.dist(distances_km))
    avg_km <- round(mean(as.dist(distances_km)),4)
    maxx_km <- max(as.dist(distances_km))
    m_cluster_summaries_infrastructure[j,] <- c(i, event_count, minm, avg, maxx, std, minm_date, maxx_date, minm_km, avg_km, maxx_km)
    ##    print( m_cluster_summaries_infrastructure[j,])
    j <- j+1
}
m_cluster_summaries_infrastructure <- as.data.frame(m_cluster_summaries_infrastructure)
