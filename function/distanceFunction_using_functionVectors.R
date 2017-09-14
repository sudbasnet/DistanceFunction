distanceFunction_using_vectors <- function(m3, dmax, tmax, var_count = 0, fun=distHaversine, weight_spatiotemporal =0.5, spatialORtemporal = 'both') {

	if (weight_spatiotemporal > 1 || weight_spatiotemporal < 0){
		stop("weights for spatiotemporal must be between 0 and 1.")
	}
	weight_socioeconomic <- 1 - weight_spatiotemporal

	prepareArguments <- function(events, rowNum) {
		m <- ncol(events)
		# cols_not_variables <- which(colnames(events)=="spatialres.y")
		number_of_variables <- var_count
		cols_not_variables <- m - var_count
		# print(paste("number_of_variables: ",number_of_variables))
		event_length <- 4 + number_of_variables # first 4 parameters are: population, event_startdate, event_enddate, category
		event_arg <- NA
		event_arg[1] <- events$p[rowNum]
		event_arg[2] <- as.character(events$event_start[rowNum],format="%Y-%m-%d")
		event_arg[3] <- as.character(events$event_end[rowNum],format="%Y-%m-%d")
		event_arg[4] <- events$eventCategory[rowNum]
		if (event_length > 4) {
			for (x in 1:number_of_variables){
				ln_event <- length(event_arg)
				event_arg[ln_event + 1] <- as.vector(events[rowNum, (cols_not_variables + x)])
			}
		}
		return(event_arg)
	}

	points <- .pointsToMatrix(m3)
	events <- m3@data
	n <- nrow(m3)
	m <- ncol(m3)
	distanceMatrix_spatiotemporal <- matrix(0, ncol=n, nrow=n)
	distanceMatrix_socioeconomic <- matrix(0, ncol=n, nrow=n)
	# distanceMatrix_final <- matrix(0, ncol=n, nrow=n)

	for (i in 2:n) {
		for (j in 1:(i-1)){
			points1 <- .pointsToMatrix(m3[i,])
			points2 <- .pointsToMatrix(m3[j,])
			event1 <- prepareArguments(events, i)
			event2 <- prepareArguments(events, j)
			# print(paste("i,j",i,j))
			distance_event1_event2 <- distanceFunction_vectors(event1, points1, event2, points2, dmax, tmax)
			distanceMatrix_spatiotemporal[i,j] <- distance_event1_event2[1]
			distanceMatrix_socioeconomic[i,j] <- distance_event1_event2[2]
			# print(distance_event1_event2)
		}
	}
	distanceMatrix_spatiotemporal <- distanceMatrix_spatiotemporal+t(distanceMatrix_spatiotemporal)
	distanceMatrix_socioeconomic <- distanceMatrix_socioeconomic + t(distanceMatrix_socioeconomic)

	if(spatialORtemporal=='spatiotemporal'){
		distanceMatrix_final <- distanceMatrix_spatiotemporal
	} else if(spatialORtemporal=='socioeconomic'){
		distanceMatrix_final <- distanceMatrix_socioeconomic
	} else {
		distanceMatrix_final <- (distanceMatrix_spatiotemporal * weight_spatiotemporal) + ((1 - weight_spatiotemporal) * distanceMatrix_socioeconomic)
	}
	# distanceMatrix_final <- distanceMatrix_final+t(distanceMatrix_final)
	return(distanceMatrix_final)
}

    # user   system  elapsed 
# 3543.477   25.446 4506.223 