plotClusterStructure <- function (dbscanObject){
	ggplot(setNames(as.data.frame(dbscanObject$cluster[dbscanObject$cluster!=0]),c("cluster")), aes(x=cluster)) + geom_histogram(binwidth = 1) + xlab("cluster number") + ylab("number of events")
}