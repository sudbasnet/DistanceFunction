# distance from centeroid:

dbscan_events_cluster_23_test001 <- event_c[dbscan_events_test001$cluster != 1 & dbscan_events_test001$cluster != 0,]
dbscan_events_cluster_13_test001 <- event_c[dbscan_events_test001$cluster != 2 & dbscan_events_test001$cluster != 0,]
dbscan_events_cluster_12_test001 <- event_c[dbscan_events_test001$cluster != 3 & dbscan_events_test001$cluster != 0,]

# n <- nrow(event_c[dbscan_events_test001$cluster == 1,])
n <- nrow(dbscan_events_cluster_23_test001)

points_centeroid <- .pointsToMatrix(centeroid_cluster1_test001[1,])  
event_centeroid <- NA
event_centeroid[1] <- centeroid_cluster1_test001$p[1]
event_centeroid[2] <- as.character(centeroid_cluster1_test001$event_start[1],format = "%Y-%m-%d")
event_centeroid[3] <- as.character(centeroid_cluster1_test001$event_end[1],format = "%Y-%m-%d")
event_centeroid[4] <- 'any'
event_centeroid[5] <- centeroid_cluster1_test001$literacyRate 
event_centeroid[6] <- centeroid_cluster1_test001$workerPop

distance_inter_centeroid_cluster1 <- matrix(0, ncol=1, nrow=n)

for(x in 1:n){
	points2 <- .pointsToMatrix(dbscan_events_cluster_23_test001[i,])
	event2 <- NA
	event2[1] <- dbscan_events_cluster_23_test001$p[x]
	event2[2] <- as.character(dbscan_events_cluster_23_test001$event_start[x],format = "%Y-%m-%d")
	event2[3] <- as.character(dbscan_events_cluster_23_test001$event_end[x],format = "%Y-%m-%d")
	event2[4] <- dbscan_events_cluster_23_test001$eventCategory[x]
	event2[5] <- dbscan_events_cluster_23_test001$literacyRate[x]
	event2[6] <- dbscan_events_cluster_23_test001$workerPop[x]
	distance_inter_centeroid_cluster1[x] <- distanceFunction_vectors(event_centeroid, points_centeroid, event2, points2, dmax=100, tmax=30)
}

n <- nrow(dbscan_events_cluster_13_test001)

points_centeroid <- .pointsToMatrix(centeroid_cluster2_test001[1,])  
event_centeroid <- NA
event_centeroid[1] <- centeroid_cluster2_test001$p[1]
event_centeroid[2] <- as.character(centeroid_cluster2_test001$event_start[1],format = "%Y-%m-%d")
event_centeroid[3] <- as.character(centeroid_cluster2_test001$event_end[1],format = "%Y-%m-%d")
event_centeroid[4] <- 'any'
event_centeroid[5] <- centeroid_cluster2_test001$literacyRate 
event_centeroid[6] <- centeroid_cluster2_test001$workerPop

distance_inter_centeroid_cluster2 <- matrix(0, ncol=1, nrow=n)

for(x in 1:n){
	points2 <- .pointsToMatrix(dbscan_events_cluster_13_test001[i,])
	event2 <- NA
	event2[1] <- dbscan_events_cluster_13_test001$p[x]
	event2[2] <- as.character(dbscan_events_cluster_13_test001$event_start[x],format = "%Y-%m-%d")
	event2[3] <- as.character(dbscan_events_cluster_13_test001$event_end[x],format = "%Y-%m-%d")
	event2[4] <- dbscan_events_cluster_13_test001$eventCategory[x]
	event2[5] <- dbscan_events_cluster_13_test001$literacyRate[x]
	event2[6] <- dbscan_events_cluster_13_test001$workerPop[x]
	distance_inter_centeroid_cluster2[x] <- distanceFunction_vectors(event_centeroid, points_centeroid, event2, points2, dmax=100, tmax=30)
}

n <- nrow(dbscan_events_cluster_12_test001)

points_centeroid <- .pointsToMatrix(centeroid_cluster3_test001[1,])  
event_centeroid <- NA
event_centeroid[1] <- centeroid_cluster3_test001$p[1]
event_centeroid[2] <- as.character(centeroid_cluster3_test001$event_start[1],format = "%Y-%m-%d")
event_centeroid[3] <- as.character(centeroid_cluster3_test001$event_end[1],format = "%Y-%m-%d")
event_centeroid[4] <- 'any'
event_centeroid[5] <- centeroid_cluster3_test001$literacyRate 
event_centeroid[6] <- centeroid_cluster3_test001$workerPop

distance_inter_centeroid_cluster3 <- matrix(0, ncol=1, nrow=n)

for(x in 1:n){
	points2 <- .pointsToMatrix(dbscan_events_cluster_12_test001[i,])
	event2 <- NA
	event2[1] <- dbscan_events_cluster_12_test001$p[x]
	event2[2] <- as.character(dbscan_events_cluster_12_test001$event_start[x],format = "%Y-%m-%d")
	event2[3] <- as.character(dbscan_events_cluster_12_test001$event_end[x],format = "%Y-%m-%d")
	event2[4] <- dbscan_events_cluster_12_test001$eventCategory[x]
	event2[5] <- dbscan_events_cluster_12_test001$literacyRate[x]
	event2[6] <- dbscan_events_cluster_12_test001$workerPop[x]
	distance_inter_centeroid_cluster3[x] <- distanceFunction_vectors(event_centeroid, points_centeroid, event2, points2, dmax=100, tmax=30)
}



event1_centeroid <- NA
event1_centeroid[1] <- centeroid_cluster1_test001$p[1]
event1_centeroid[2] <- as.character(centeroid_cluster1_test001$event_start[1],format = "%Y-%m-%d")
event1_centeroid[3] <- as.character(centeroid_cluster1_test001$event_end[1],format = "%Y-%m-%d")
event1_centeroid[4] <- 'any'
event1_centeroid[5] <- centeroid_cluster1_test001$literacyRate 
event1_centeroid[6] <- centeroid_cluster1_test001$workerPop
points_1 <- .pointsToMatrix(centeroid_cluster1_test001[1,])

event2_centeroid <- NA
event2_centeroid[1] <- centeroid_cluster2_test001$p[1]
event2_centeroid[2] <- as.character(centeroid_cluster2_test001$event_start[1],format = "%Y-%m-%d")
event2_centeroid[3] <- as.character(centeroid_cluster2_test001$event_end[1],format = "%Y-%m-%d")
event2_centeroid[4] <- 'any'
event2_centeroid[5] <- centeroid_cluster2_test001$literacyRate 
event2_centeroid[6] <- centeroid_cluster2_test001$workerPop
points_2 <- .pointsToMatrix(centeroid_cluster2_test001[1,])

event3_centeroid <- NA
event3_centeroid[1] <- centeroid_cluster3_test001$p[1]
event3_centeroid[2] <- as.character(centeroid_cluster3_test001$event_start[1],format = "%Y-%m-%d")
event3_centeroid[3] <- as.character(centeroid_cluster3_test001$event_end[1],format = "%Y-%m-%d")
event3_centeroid[4] <- 'any'
event3_centeroid[5] <- centeroid_cluster3_test001$literacyRate 
event3_centeroid[6] <- centeroid_cluster3_test001$workerPop
points_3 <- .pointsToMatrix(centeroid_cluster3_test001[1,])

distance_12 <- distanceFunction_vectors(event1_centeroid, points_1, event2_centeroid, points_2, dmax=100, tmax=30)
distance_23 <- distanceFunction_vectors(event2_centeroid, points_2, event3_centeroid, points_3, dmax=100, tmax=30)
distance_13 <- distanceFunction_vectors(event1_centeroid, points_1, event3_centeroid, points_3, dmax=100, tmax=30)


--------------------

# distance from centeroid:

n <- nrow(cluster3_test001[dbscan_events_test001$cluster == 3,])
m <- nrow(cluster3_test001[dbscan_events_test001$cluster == 3,])

points_centeroid <- .pointsToMatrix(centeroid_cluster3_test001[1,])  
event_centeroid <- NA
event_centeroid[1] <- centeroid_cluster3_test001$p[1]
event_centeroid[2] <- as.character(centeroid_cluster3_test001$event_start[1],format = "%Y-%m-%d")
event_centeroid[3] <- as.character(centeroid_cluster3_test001$event_end[1],format = "%Y-%m-%d")
event_centeroid[4] <- 'any'
event_centeroid[5] <- centeroid_cluster3_test001$literacyRate 
event_centeroid[6] <- centeroid_cluster3_test001$workerPop

distance_centeroid_cluster3 <- matrix(0, ncol=1, nrow=n)

for(x in 1:n){
	points2 <- .pointsToMatrix(cluster3_test001[i,])
	event2 <- NA
	event2[1] <- cluster3_test001$p[x]
	event2[2] <- as.character(cluster3_test001$event_start[x],format = "%Y-%m-%d")
	event2[3] <- as.character(cluster3_test001$event_end[x],format = "%Y-%m-%d")
	event2[4] <- cluster3_test001$eventCategory[x]
	event2[5] <- cluster3_test001$literacyRate[x]
	event2[6] <- cluster3_test001$workerPop[x]
	distance_centeroid_cluster3[x] <- distanceFunction_vectors(event_centeroid, points_centeroid, event2, points2, dmax=100, tmax=30)
}

------------------------------

# distance from centeroid:

n <- nrow(event_c[dbscan_events_test001$cluster == 3,])

points_centeroid <- .pointsToMatrix(centeroid_cluster3_test001[1,])  
event_centeroid <- NA
event_centeroid[1] <- centeroid_cluster3_test001$p[1]
event_centeroid[2] <- as.character(centeroid_cluster3_test001$event_start[1],format = "%Y-%m-%d")
event_centeroid[3] <- as.character(centeroid_cluster3_test001$event_end[1],format = "%Y-%m-%d")
event_centeroid[4] <- 'any'
event_centeroid[5] <- centeroid_cluster3_test001$literacyRate 
event_centeroid[6] <- centeroid_cluster3_test001$workerPop

distance_centeroid_cluster3 <- matrix(0, ncol=1, nrow=n)

for(x in 1:n){
	points2 <- .pointsToMatrix(cluster3_test001[i,])
	event2 <- NA
	event2[1] <- cluster3_test001$p[x]
	event2[2] <- as.character(cluster3_test001$event_start[x],format = "%Y-%m-%d")
	event2[3] <- as.character(cluster3_test001$event_end[x],format = "%Y-%m-%d")
	event2[4] <- cluster3_test001$eventCategory[x]
	event2[5] <- cluster3_test001$literacyRate[x]
	event2[6] <- cluster3_test001$workerPop[x]
	distance_centeroid_cluster3[x] <- distanceFunction_vectors(event_centeroid, points_centeroid, event2, points2, dmax=100, tmax=30)
}


----------------------------
# distance from centeroid:

n <- nrow(cluster1_test001)
m <- nrow(cluster3_test001)

distance_between_clusters_12 <- matrix(0, ncol=m, nrow=n)

for(x in 1:n){
	event1 <- NA
	event1[1] <- cluster1_test001$p[x]
	event1[2] <- as.character(cluster1_test001$event_start[x],format = "%Y-%m-%d")
	event1[3] <- as.character(cluster1_test001$event_end[x],format = "%Y-%m-%d")
	event1[4] <- 'any'
	event1[5] <- cluster1_test001$literacyRate[x]
	event1[6] <- cluster1_test001$workerPop[x]
	points1 <- .pointsToMatrix(cluster1_test001[x,])

	for(y in 1:m){
		points3 <- .pointsToMatrix(cluster3_test001[y,])
		event3 <- NA
		event3[1] <- cluster3_test001$p[y]
		event3[2] <- as.character(cluster3_test001$event_start[y],format = "%Y-%m-%d")
		event3[3] <- as.character(cluster3_test001$event_end[y],format = "%Y-%m-%d")
		event3[4] <- cluster3_test001$eventCategory[y]
		event3[5] <- cluster3_test001$literacyRate[y]
		event3[6] <- cluster3_test001$workerPop[y]

		distance_between_clusters_13[x,y] <- distanceFunction_vectors(event1, points1, event3, points3, dmax=100, tmax=30)
	}
}
---------------------------

# distance from centeroid:

n <- nrow(cluster2_test001)
m <- nrow(cluster3_test001)

distance_between_clusters_23 <- matrix(0, ncol=m, nrow=n)

for(x in 1:n){
	event2 <- NA
	event2[1] <- cluster2_test001$p[x]
	event2[2] <- as.character(cluster2_test001$event_start[x],format = "%Y-%m-%d")
	event2[3] <- as.character(cluster2_test001$event_end[x],format = "%Y-%m-%d")
	event2[4] <- 'any'
	event2[5] <- cluster2_test001$literacyRate[x]
	event2[6] <- cluster2_test001$workerPop[x]
	points2 <- .pointsToMatrix(cluster2_test001[x,])

	for(y in 1:m){
		points3 <- .pointsToMatrix(cluster3_test001[y,])
		event3 <- NA
		event3[1] <- cluster3_test001$p[y]
		event3[2] <- as.character(cluster3_test001$event_start[y],format = "%Y-%m-%d")
		event3[3] <- as.character(cluster3_test001$event_end[y],format = "%Y-%m-%d")
		event3[4] <- cluster3_test001$eventCategory[y]
		event3[5] <- cluster3_test001$literacyRate[y]
		event3[6] <- cluster3_test001$workerPop[y]

		distance_between_clusters_23[x,y] <- distanceFunction_vectors(event2, points2, event3, points3, dmax=100, tmax=30)
	}
}
-------------------------