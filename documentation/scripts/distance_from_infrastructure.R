# First I collect the matrix of distances of each event with all the different kinds of infrastructures
# I create one matrix for each type of infrastructure.
# during the calculations, I also log the minimum distance from the calculated distances for each event. This information is in the 2nd column of the matrix.
# I also log the number of infrastructure elements within a distance of 10 km (10000 m). This information is in the 3rd column of the matrix.
n <- nrow(data2_india_2014_northeast)
m <- nrow(poi_northeast_police)
count <- 0
m_police_distance <- matrix( 0, ncol = m+3, nrow = n)
points1 <- .pointsToMatrix(data2_india_2014_northeast)
points2 <- .pointsToMatrix(poi_northeast_police)
for(i in 1:n){
	m_police_distance[i,1] <- data2_india_2014_northeast@data[i,1]
	for(j in 1:m){
		d <- distHaversine(points1[i,], points2[j,])
		m_police_distance[i,(j+3)] <- d
		if (d <= 10000){count <- count+1}
	}
	m_police_distance[i,2] <- min(m_police_distance[i,c(4:(m+3))])
	m_police_distance[i,3] <- count
}
#-------------------------------------------
# n <- nrow(data2_india_2014_northeast)
m <- nrow(poi_northeast_postal)
count <- 0
m_postal_distance <- matrix( 0, ncol = m+3, nrow = n)
points1 <- .pointsToMatrix(data2_india_2014_northeast)
points2 <- .pointsToMatrix(poi_northeast_postal)
for(i in 1:n){
	m_postal_distance[i,1] <- data2_india_2014_northeast@data[i,1]
	for(j in 1:m){
		d <- distHaversine(points1[i,], points2[j,])
		m_postal_distance[i,(j+3)] <- d
		if (d <= 10000){count <- count+1}
	}
	m_postal_distance[i,2] <- min(m_postal_distance[i,c(4:(m+3))])
	m_postal_distance[i,3] <- count
}
#-------------------------------------------
# n <- nrow(data2_india_2014_northeast)
m <- nrow(poi_northeast_hospital)
count <- 0
m_hospital_distance <- matrix( 0, ncol = m+3, nrow = n)
points1 <- .pointsToMatrix(data2_india_2014_northeast)
points2 <- .pointsToMatrix(poi_northeast_hospital)
for(i in 1:n){
	m_hospital_distance[i,1] <- data2_india_2014_northeast@data[i,1]
	for(j in 1:m){
		d <- distHaversine(points1[i,], points2[j,])
		m_hospital_distance[i,(j+3)] <- d
		if (d <= 10000){count <- count+1}
	}
	m_hospital_distance[i,2] <- min(m_hospital_distance[i,c(4:(m+3))])
	m_hospital_distance[i,3] <- count
}
#-------------------------------------------
# n <- nrow(data2_india_2014_northeast)
m <- nrow(poi_northeast_university)
count <- 0
m_university_distance <- matrix( 0, ncol = m+3, nrow = n)
points1 <- .pointsToMatrix(data2_india_2014_northeast)
points2 <- .pointsToMatrix(poi_northeast_university)
for(i in 1:n){
	m_university_distance[i,1] <- data2_india_2014_northeast@data[i,1]
	for(j in 1:m){
		d <- distHaversine(points1[i,], points2[j,])
		m_university_distance[i,(j+3)] <- d
		if (d <= 10000){count <- count+1}
	}
	m_university_distance[i,2] <- min(m_university_distance[i,c(4:(m+3))])
	m_university_distance[i,3] <- count
}
#-------------------------------------------
# n <- nrow(data2_india_2014_northeast)
m <- nrow(poi_northeast_college)
count <- 0
m_college_distance <- matrix( 0, ncol = m+3, nrow = n)
points1 <- .pointsToMatrix(data2_india_2014_northeast)
points2 <- .pointsToMatrix(poi_northeast_college)
for(i in 1:n){
	m_college_distance[i,1] <- data2_india_2014_northeast@data[i,1]
	for(j in 1:m){
		d <- distHaversine(points1[i,], points2[j,])
		m_college_distance[i,(j+3)] <- d
		if (d <= 10000){count <- count+1}
	}
	m_college_distance[i,2] <- min(m_college_distance[i,c(4:(m+3))])
	m_college_distance[i,3] <- count
}
#-------------------------------------------
# n <- nrow(data2_india_2014_northeast)
m <- nrow(poi_northeast_school)
count <- 0
m_school_distance <- matrix( 0, ncol = m+3, nrow = n)
points1 <- .pointsToMatrix(data2_india_2014_northeast)
points2 <- .pointsToMatrix(poi_northeast_school)
for(i in 1:n){
	m_school_distance[i,1] <- data2_india_2014_northeast@data[i,1]
	for(j in 1:m){
		d <- distHaversine(points1[i,], points2[j,])
		m_school_distance[i,(j+3)] <- d
		if (d <= 10000){count <- count+1}
	}
	m_school_distance[i,2] <- min(m_school_distance[i,c(4:(m+3))])
	m_school_distance[i,3] <- count
}
#-------------------------------------------
# Now for the ease of calculation, we create a new matrix (and then data.frame) to store the summary of the information above.
m_infrastructure_density_summary <- matrix( 0, ncol = 7, nrow = n)
m_infrastructure_proximity_summary <- matrix( 0, ncol = 7, nrow = n)
colnames(m_infrastructure_proximity_summary) <- c("uniqueid","police","postal","hospital","university","college","school")
colnames(m_infrastructure_density_summary) <- c("uniqueid","police","postal","hospital","university","college","school")
for (i in 1:n){
	m_infrastructure_proximity_summary[i,1] <- m_police_distance[i,1]
	m_infrastructure_proximity_summary[i,2] <- m_police_distance[i,2]
	m_infrastructure_proximity_summary[i,3] <- m_postal_distance[i,2]
	m_infrastructure_proximity_summary[i,4] <- m_hospital_distance[i,2]
	m_infrastructure_proximity_summary[i,5] <- m_university_distance[i,2]
	m_infrastructure_proximity_summary[i,6] <- m_college_distance[i,2]
	m_infrastructure_proximity_summary[i,7] <- m_school_distance[i,2]
	m_infrastructure_density_summary[i,1] <- m_police_distance[i,1]
	m_infrastructure_density_summary[i,2] <- m_police_distance[i,3]
	m_infrastructure_density_summary[i,3] <- m_postal_distance[i,3]
	m_infrastructure_density_summary[i,4] <- m_hospital_distance[i,3]
	m_infrastructure_density_summary[i,5] <- m_university_distance[i,3]
	m_infrastructure_density_summary[i,6] <- m_college_distance[i,3]
	m_infrastructure_density_summary[i,7] <- m_school_distance[i,3]
}
m_infrastructure_proximity_summary <- as.data.frame(m_infrastructure_proximity_summary)
m_infrastructure_density_summary <- as.data.frame(m_infrastructure_density_summary)
#--------------------------------------------
# Now, finally we normalize the data using feature scaling.
m_infrastructure_density_summary_normalized <- m_infrastructure_density_summary
m_infrastructure_proximity_summary_normalized <- m_infrastructure_proximity_summary
for (i in 2:7){
	minimum <- min(m_infrastructure_density_summary_normalized[,i])
	maximum <- max(m_infrastructure_density_summary_normalized[,i])
	m_infrastructure_density_summary_normalized[,i] <- (m_infrastructure_density_summary_normalized[,i] - minimum)/(maximum - minimum)
	minimum_ <- min(m_infrastructure_proximity_summary_normalized[,i])
	maximum_ <- max(m_infrastructure_proximity_summary_normalized[,i])
	m_infrastructure_proximity_summary_normalized[,i] <- (m_infrastructure_proximity_summary_normalized[,i] - minimum_)/(maximum_ - minimum_)
}
#---------------------------------------------






