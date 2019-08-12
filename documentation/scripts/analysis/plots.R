clusters <- c(0,29,8,3,60,61,69)
for(c in clusters){
    jpeg_name <- paste("./plots/clustering_results_infrastructure_cluster",c, sep = "")
    objects <- nrow(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_infrastructure_max10$cluster==c,])
    epss <- dbscan_india_2014_northeast_spatiotemporal_infrastructure_max10$eps
    jpeg(paste(jpeg_name,".jpg", sep = ""), 3000, 3000, res = 300)
    plot(map_india_northeast_d, 
         main = paste("Northeastern India\nCluster No. ", c , sep=""), 
         xlab = paste("spatiotemporal and infrastructural clustering, ", objects ," events, eps: ", epss, ", minPts: 4", sep = ""), 
         lty = 3)
    plot(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_infrastructure_max10$cluster==c,], col=4, pch=16, add=T)
    dev.off()
}


clusters <- c(0,4,25,12,96,99,100)
for(c in clusters){
    jpeg_name <- paste("./plots/clustering_results_spatio_socio_infrastructure_cluster",c, sep = "")
    objects <- nrow(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==c,])
    epss <- dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$eps
    jpeg(paste(jpeg_name,".jpg", sep = ""), 3000, 3000, res = 300)
    plot(map_india_northeast_d,
    main = paste("Northeastern India\nCluster No. ", c , sep=""),
    xlab = paste("spatiotemporal and infrastructural clustering, ", objects ," events, eps: ", epss, ", minPts: 4", sep = ""),
    lty = 3)
    plot(data3_india_2014_northeast[dbscan_india_2014_northeast_spatiotemporal_socioeconomic_infrastructure_max10$cluster==c,], col=4, pch=16, add=T)
    dev.off()
}
