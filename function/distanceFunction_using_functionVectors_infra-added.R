distanceFunction_using_vectors_infra <- function(m3, dmax, tmax, var_count = 0, infra_count = 0, fun=distHaversine, weight_spatiotemporal =0.5, weight_socioeconomic=0.5, spatialORsocioORinfra = 'all') {

	if (weight_spatiotemporal > 1 || weight_spatiotemporal < 0){
		stop("weights for spatiotemporal must be between 0 and 1.")
	}

	prepareArguments <- function(events, rowNum) {
		m <- ncol(events)
		# cols_not_variables <- which(colnames(events)=="spatialres.y")
		number_of_variables <- var_count
		number_of_infrastructures <- infra_count
		cols_not_variables <- m - (var_count + (2 * infra_count))
		# print(paste("number_of_variables: ",number_of_variables))
		event_length <- 4 + number_of_variables + (2 * number_of_infrastructures) # first 4 parameters are: population, event_startdate, event_enddate, category
		event_arg <- NA
		event_arg[1] <- events$p[rowNum]
		event_arg[2] <- as.character(events$event_start[rowNum],format="%Y-%m-%d")
		event_arg[3] <- as.character(events$event_end[rowNum],format="%Y-%m-%d")
		event_arg[4] <- events$eventCategory[rowNum]
		if (number_of_variables > 0 || number_of_infrastructures > 0) {
			for (x in 1:number_of_variables){
				ln_event <- length(event_arg)
				event_arg[ln_event + 1] <- as.vector(events[rowNum, (cols_not_variables + x)])
			}
			for (y in 1:number_of_infrastructures){ # this is for the proximity type infrastructure
				ln_event <- length(event_arg)
				event_arg[ln_event + 1] <- as.vector(events[rowNum, (cols_not_variables + number_of_variables + y)])
			}
			for (z in 1:number_of_infrastructures){ # this is for the density type infrastructure
				ln_event <- length(event_arg)
				event_arg[ln_event + 1] <- as.vector(events[rowNum, (cols_not_variables + number_of_variables + number_of_infrastructures + z)])
			}
		}
		return(event_arg)
	}

	# points <- .pointsToMatrix(m3)
	events <- m3@data
	n <- nrow(m3)
	m <- ncol(m3)
	distanceMatrix_spatiotemporal <- matrix(0, ncol=n, nrow=n)
	distanceMatrix_socioeconomic <- matrix(0, ncol=n, nrow=n)
	distanceMatrix_infrastructure <- matrix(0, ncol=n, nrow=n)
	# distanceMatrix_final <- matrix(0, ncol=n, nrow=n)

	for (i in 2:n) {
		for (j in 1:(i-1)){
			points1 <- .pointsToMatrix(m3[i,])
			points2 <- .pointsToMatrix(m3[j,])
			event1 <- prepareArguments(events, i)
			event2 <- prepareArguments(events, j)
			# print(paste("i,j",i,j))
			distance_event1_event2 <- distanceFunction_vectors_infra(event1, points1, event2, points2, dmax, tmax, var_count, infra_count)
			distanceMatrix_spatiotemporal[i,j] <- distance_event1_event2[1]
			distanceMatrix_socioeconomic[i,j] <- distance_event1_event2[2]
			distanceMatrix_infrastructure[i,j] <- distance_event1_event2[3]
			# print(distance_event1_event2)
		}
	}
	distanceMatrix_spatiotemporal <- distanceMatrix_spatiotemporal+t(distanceMatrix_spatiotemporal)
	distanceMatrix_socioeconomic <- distanceMatrix_socioeconomic + t(distanceMatrix_socioeconomic)
	distanceMatrix_infrastructure <- distanceMatrix_infrastructure + t(distanceMatrix_infrastructure)
	
	distanceMatrix_spatiotemporal_recent <<- distanceMatrix_spatiotemporal
	distanceMatrix_socioeconomic_recent <<- distanceMatrix_socioeconomic
	distanceMatrix_infrastructure_recent <<- distanceMatrix_infrastructure
	assign(paste("distanceMatrix_spatiotemporal",Sys.Date(),sep="_"), distanceMatrix_spatiotemporal_recent)
	assign(paste("distanceMatrix_socioeconomic",Sys.Date(),sep="_"), distanceMatrix_socioeconomic_recent)
	assign(paste("distanceMatrix_infrastructure",Sys.Date(),sep="_"),  distanceMatrix_infrastructure_recent)

	if(spatialORsocioORinfra=='spatiotemporal'){
		distanceMatrix_final <- distanceMatrix_spatiotemporal
	} else if(spatialORsocioORinfra=='socioeconomic'){
		distanceMatrix_final <- distanceMatrix_socioeconomic
	} else if(spatialORsocioORinfra == 'infrastructure'){
		distanceMatrix_final <- distanceMatrix_infrastructure
	} else if(spatialORsocioORinfra == 'spatiotemporal_infrastructure'){
		distanceMatrix_final <- (distanceMatrix_spatiotemporal * 0.5) + (0.5 * distanceMatrix_infrastructure)
	} else if(spatialORsocioORinfra == 'all'){
		weight_infrastructure <- 1 - (weight_spatiotemporal + weight_socioeconomic)
		print(paste("weight_infrastructure",weight_infrastructure))
		distanceMatrix_final <- (distanceMatrix_spatiotemporal * weight_spatiotemporal) + (weight_socioeconomic * distanceMatrix_socioeconomic) + (weight_infrastructure * distanceMatrix_infrastructure)
	} else {
		stop("weights and spatialORsocioORinfra do not make sense.")
	}
	# distanceMatrix_final <- distanceMatrix_final+t(distanceMatrix_final)
	return(distanceMatrix_final)
}