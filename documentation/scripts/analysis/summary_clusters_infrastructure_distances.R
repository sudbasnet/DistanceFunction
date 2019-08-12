infrastructures <- c('police','hospital','postal','school','college','university')
clusters <- c(1:8) ## need to give this manually every time, these are the clusters to be checked
n <- length(infrastructures) ## number of infrastructures
m <- length(clusters) ## number of clusters that we are examining
count <- 0
m_infraDistanceSummary_clusters_infra <- matrix( 0, ncol = 6, nrow = m * n)
colnames(m_infraDistanceSummary_clusters_infra) <- c('Cluster', 'Infrastructure', 'Mean (proximity)', 'Standard Deviation (proximity)', 'Mean (density)', 'Standard Deviation (density)');
round <- 0
for(i in 1:m){
    c <- clusters[i]
    for(j in 1:n){
        infrastructure_prox <- paste(infrastructures[j],'_proximity',sep='') # col names of the data.frame data3_india_2014_northeast
        infrastructure_den <- paste(infrastructures[j],'_density',sep='') # col names of the data.frame data3_india_2014_northeast
        avg_prox <- round(mean(data3_india_2014_northeast@data[dbscan_india_2014_northeast_infrastructure_max10$cluster==c,infrastructure_prox]),4)
        sd_prox <- round(sd(data3_india_2014_northeast@data[dbscan_india_2014_northeast_infrastructure_max10$cluster==c,infrastructure_prox]),4)
        avg_den <- round(mean(data3_india_2014_northeast@data[dbscan_india_2014_northeast_infrastructure_max10$cluster==c,infrastructure_den]),4)
        sd_den <- round(sd(data3_india_2014_northeast@data[dbscan_india_2014_northeast_infrastructure_max10$cluster==c,infrastructure_den]),4)
        m_infraDistanceSummary_clusters_infra[round+j,1] <- c
        m_infraDistanceSummary_clusters_infra[round+j,2] <- infrastructures[j]
        m_infraDistanceSummary_clusters_infra[round+j,3] <- avg_prox
        m_infraDistanceSummary_clusters_infra[round+j,4] <- sd_prox
        m_infraDistanceSummary_clusters_infra[round+j,5] <- avg_den
        m_infraDistanceSummary_clusters_infra[round+j,6] <- sd_den
    }
    ## next round
    round <- round + n
}
## not convert the matrix into a data.frame and write it to an excel file in the subfolder "summary_xlsx"
m_infraDistanceSummary_clusters_infra <- as.data.frame(m_infraDistanceSummary_clusters_infra)
write.xlsx(m_infraDistanceSummary_clusters_infra, "./summary_xlsx/m_infraDistanceSummary_clusters_infra.xlsx")

infrastructures <- c('police','hospital','postal','school','college','university')
clusters <- c(29,8,3,60,61,69) ## need to give this manually every time, these are the clusters to be checked
n <- length(infrastructures) ## number of infrastructures
m <- length(clusters) ## number of clusters that we are examining
count <- 0
m_infraDistanceSummary_clusters_infra_spatial <- matrix( 0, ncol = 6, nrow = m * n)
colnames(m_infraDistanceSummary_clusters_infra_spatial) <- c('Cluster', 'Infrastructure', 'Mean (proximity)', 'Standard Deviation (proximity)', 'Mean (density)', 'Standard Deviation (density)');
round <- 0
for(i in 1:m){
	c <- clusters[i]
	for(j in 1:n){
        infrastructure_prox <- paste(infrastructures[j],'_proximity',sep='') # col names of the data.frame data3_india_2014_northeast
		infrastructure_den <- paste(infrastructures[j],'_density',sep='') # col names of the data.frame data3_india_2014_northeast
		avg_prox <- round(mean(data3_india_2014_northeast@data[dbscan_india_2014_northeast_spatiotemporal_infrastructure_max10$cluster==c,infrastructure_prox]),4)
		sd_prox <- round(sd(data3_india_2014_northeast@data[dbscan_india_2014_northeast_spatiotemporal_infrastructure_max10$cluster==c,infrastructure_prox]),4)
		avg_den <- round(mean(data3_india_2014_northeast@data[dbscan_india_2014_northeast_spatiotemporal_infrastructure_max10$cluster==c,infrastructure_den]),4)
		sd_den <- round(sd(data3_india_2014_northeast@data[dbscan_india_2014_northeast_spatiotemporal_infrastructure_max10$cluster==c,infrastructure_den]),4)
		m_infraDistanceSummary_clusters_infra_spatial[round+j,1] <- c
		m_infraDistanceSummary_clusters_infra_spatial[round+j,2] <- infrastructures[j]
		m_infraDistanceSummary_clusters_infra_spatial[round+j,3] <- avg_prox
		m_infraDistanceSummary_clusters_infra_spatial[round+j,4] <- sd_prox
		m_infraDistanceSummary_clusters_infra_spatial[round+j,5] <- avg_den
		m_infraDistanceSummary_clusters_infra_spatial[round+j,6] <- sd_den
	}
    ## next round
	round <- round + n
}
## not convert the matrix into a data.frame and write it to an excel file in the subfolder "summary_xlsx"
m_infraDistanceSummary_clusters_infra_spatial <- as.data.frame(m_infraDistanceSummary_clusters_infra_spatial)
write.xlsx(m_infraDistanceSummary_clusters_infra_spatial, "./summary_xlsx/m_infraDistanceSummary_clusters_infra_spatial.xlsx")

## now we do the same thing for combined distances

infrastructures <- c('police','hospital','postal','school','college','university')
clusters <- c(0,4,25,12,96,99,100) ## need to give this manually every time, these are the clusters to be checked
n <- length(infrastructures) ## number of infrastructures
m <- length(clusters) ## number of clusters that we are examining
count <- 0
m_infraDistanceSummary_clusters_infra_spatial_socio <- matrix( 0, ncol = 6, nrow = m * n)
colnames(m_infraDistanceSummary_clusters_infra_spatial_socio) <- c('Cluster', 'Infrastructure', 'Mean (proximity)', 'Standard Deviation (proximity)', 'Mean (density)', 'Standard Deviation (density)');
round <- 0
for(i in 1:m){
    c <- clusters[i]
    for(j in 1:n){
        infrastructure_prox <- paste(infrastructures[j],'_proximity',sep='') # col names of the data.frame data3_india_2014_northeast
        infrastructure_den <- paste(infrastructures[j],'_density',sep='') # col names of the data.frame data3_india_2014_northeast
        avg_prox <- round(mean(data3_india_2014_northeast@data[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==c,infrastructure_prox]),4)
        sd_prox <- round(sd(data3_india_2014_northeast@data[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==c,infrastructure_prox]),4)
        avg_den <- round(mean(data3_india_2014_northeast@data[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==c,infrastructure_den]),4)
        sd_den <- round(sd(data3_india_2014_northeast@data[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==c,infrastructure_den]),4)
        m_infraDistanceSummary_clusters_infra_spatial_socio[round+j,1] <- c
        m_infraDistanceSummary_clusters_infra_spatial_socio[round+j,2] <- infrastructures[j]
        m_infraDistanceSummary_clusters_infra_spatial_socio[round+j,3] <- avg_prox
        m_infraDistanceSummary_clusters_infra_spatial_socio[round+j,4] <- sd_prox
        m_infraDistanceSummary_clusters_infra_spatial_socio[round+j,5] <- avg_den
        m_infraDistanceSummary_clusters_infra_spatial_socio[round+j,6] <- sd_den
    }
    ## next round
    round <- round + n
}
## not convert the matrix into a data.frame and write it to an excel file in the subfolder "summary_xlsx"
m_infraDistanceSummary_clusters_infra_spatial_socio <- as.data.frame(m_infraDistanceSummary_clusters_infra_spatial_socio)
write.xlsx(m_infraDistanceSummary_clusters_infra_spatial_socio, "./summary_xlsx/m_infraDistanceSummary_clusters_infra_spatial_socio.xlsx")

