# this is used by the distanceFunction_processData function below
distanceFunction <-
function(event1,
points1,
event2,
points2,
dmax,
tmax,
var_count,
infra_count,
spatial_dist_fun=distHaversine,
spatialORsocioORinfra = 'all')
{
    # event will have the socioeconomic variables only [population, event_startdate, event_enddate, category, var1, var2, var3 ... ]
    # points will have lon lat in coordinates form
    # equal weights will be assigned to each variable
    # the socioeconomic variables should already be normalized, this function will only calculate the distance
    # the population density should already be normalized
    # print(weight_socioeconomic)
    
    if (length(event1) != length(event2)){
        stop("event vectors are not of the same length.")
    }
    
    d_spatial <- spatial_dist_fun(points1[1,], points2[1,])
    d_spatial <- round(d_spatial/1000 , 2)
    dspatialnormalized <- ifelse((d_spatial > dmax), 1, round(d_spatial/dmax,4))
    # print(paste("dspatialnormalized",dspatialnormalized))
    startDate_1 <- as.Date(event1[2], format="%Y-%m-%d")
    startDate_2 <- as.Date(event2[2], format="%Y-%m-%d")
    
    # print(paste("startDate_1:", startDate_1, "startDate_2:", startDate_2))
    
    main <- ifelse(startDate_1 < startDate_2, 'event1',
    ifelse(startDate_1 > startDate_2, 'event2',
    ifelse(as.numeric(event1[1]) < as.numeric(event2[1]),'event2','event1')))
    # print(paste("event1[1]",event1[1], "event2[1]", event2[1], ifelse(as.numeric(event1[1]) < as.numeric(event2[1]),'event2','event1')))
    # print(main)
    if (main == 'event1') {
        mainEvent <- event1
        secondEvent <- event2
    } else {
        secondEvent <- event1
        mainEvent <- event2
    }
    # print(paste("mainEvent:",mainEvent[1], "secondEvent:", secondEvent[1]))
    startDate_1 <- as.Date(mainEvent[2], format="%Y-%m-%d")
    endDate_1 <- as.Date(mainEvent[3], format="%Y-%m-%d")
    startDate_2 <- as.Date(secondEvent[2], format="%Y-%m-%d")
    
    pop <- round(as.numeric(mainEvent[1])/(as.numeric(mainEvent[1]) + as.numeric(secondEvent[1])),4)
    # temporal directional distance
    dtemporal_directional <- as.vector(ifelse(startDate_1 == endDate_1, startDate_2 - endDate_1, (startDate_2 - endDate_1)/(endDate_1 - startDate_1)))
    if (dtemporal_directional < 0) { #if less than 0 means overlap so make it 0
        dtemporal_directional <- 0
    } else {
        dtemporal_directional <- dtemporal_directional
    }
    
    # normalized the above distance, if distance greater than threshold, then 1 (max)
    dtemporalnormalized <- ifelse((dtemporal_directional > tmax), 1, round(dtemporal_directional/tmax,4))
    # print(paste("dtemporalnormalized",dtemporalnormalized))
    #print(paste("dtemporal_directional: ",dtemporal_directional," dspatialnormalized: ",dspatialnormalized))
    #weights based on the population density of the mainEvent
    wtemporal = pop # used to be popdensity
    wspatial = 1- pop # used to be popdensity
    # print(paste("wtemporal",wtemporal))
    # print(paste("wspatial",wspatial))
    d_spatiotemporal <- dtemporalnormalized * wtemporal + dspatialnormalized * wspatial
    # print(d_spatiotemporal)
    
    d_socioeconomic <- 0
    # print(mainEvent[5:length(mainEvent)])
    if (var_count > 0){
        var1 <- mainEvent[5:(4+var_count)]
        var2 <- secondEvent[5:(4+var_count)]
        ln <- length(var1)
        d_socioeconomic_vector <- NA
        for(i in 1:ln){
            d_socioeconomic_vector[i] <- abs(as.numeric(var1[i]) - as.numeric(var2[i]))
            d_socioeconomic <- d_socioeconomic + ( d_socioeconomic_vector[i] * round(1/ln,4) )
        }
    }
    d_infrastructure_proximity <- 0
    d_infrastructure_density <- 0
    d_infrastructure <- 0
    # print(mainEvent[5:length(mainEvent)])
    if (infra_count > 0){
        infra1_proximity <- mainEvent[(5+var_count):(4+var_count+infra_count)]
        infra2_proximity <- secondEvent[(5+var_count):(4+var_count+infra_count)]
        infra1_density <- mainEvent[(5+var_count+infra_count):(4+var_count+infra_count+infra_count)]
        infra2_density <- secondEvent[(5+var_count+infra_count):(4+var_count+infra_count+infra_count)]
        ln <- length(infra1_proximity)
        d_infrastructure_proximity_vector <- NA
        d_infrastructure_density_vector <- NA
        for(i in 1:ln){
            d_infrastructure_proximity_vector[i] <- abs(as.numeric(infra1_proximity[i]) - as.numeric(infra2_proximity[i]))
            d_infrastructure_density_vector[i] <- abs(as.numeric(infra1_density[i]) - as.numeric(infra2_density[i]))
            d_infrastructure_proximity <- d_infrastructure_proximity + ( d_infrastructure_proximity_vector[i] * round(1/ln,4) )
            d_infrastructure_density <- d_infrastructure_density + ( d_infrastructure_density_vector[i] * round(1/ln,4) )
        }
        d_infrastructure <- (0.5 * d_infrastructure_proximity) + (0.5 * d_infrastructure_density)
    }
    
    d_final <- NA
    d_final["spatiotemporal"] <- d_spatiotemporal
    d_final["socioeconomic"] <- d_socioeconomic
    d_final["infrastructural"] <- d_infrastructure
    d_final["spatial_km"] <- d_spatial
    d_final["spatial_normalized"] <- dspatialnormalized
    d_final["temporal_days"] <- dtemporal_directional
    d_final["temporal_normalized"] <- dtemporalnormalized
    d_final["wtemporal"] <- wtemporal
    d_final["wspatial"] <- wspatial
    return(d_final)
}


# provide the dataset to this part
distanceFunction_processData <-
function(
    m3,
    dmax = 10000.00,
    tmax = 30,
    var_count = 0,
    infra_count = 0,
    fun = distHaversine,
    weight_spatiotemporal = 0.33,
    weight_socioeconomic = 0.33,
    spatialORsocioORinfra = 'all') {
    
	if ((weight_spatiotemporal + weight_socioeconomic) > 1){
        stop(paste("weights are not correct, weight_spatiotemporal: ", weight_spatiotemporal, ", weight_socioeconomic: ", weight_socioeconomic, ".", sep=""))
	}

	prepareArguments <- function(events, rowNum, var_count, infra_count) {
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
	events <- m3
	n <- nrow(m3)
	m <- ncol(m3)
    distanceMatrix_spatial_km <- matrix(0, ncol=n, nrow=n)
    distanceMatrix_temporal <- matrix(0, ncol=n, nrow=n)
    distanceMatrix_spatial_weight <- matrix(0, ncol=n, nrow=n)
    distanceMatrix_temporal_weight <- matrix(0, ncol=n, nrow=n)
	distanceMatrix_spatiotemporal <- matrix(0, ncol=n, nrow=n)
	distanceMatrix_socioeconomic <- matrix(0, ncol=n, nrow=n)
	distanceMatrix_infrastructure <- matrix(0, ncol=n, nrow=n)
    distanceMatrix_spatial_normalized <- matrix(0, ncol=n, nrow=n)
    distanceMatrix_temporal_normalized <- matrix(0, ncol=n, nrow=n)
	# distanceMatrix_final <- matrix(0, ncol=n, nrow=n)

	for (i in 2:n) {
		for (j in 1:(i-1)){
			points1 <- as.matrix(m3[i,c("lon","lat")])
            points2 <- as.matrix(m3[j,c("lon","lat")])
			event1 <- prepareArguments(events, i)
			event2 <- prepareArguments(events, j)
			# print(paste("i,j",i,j))
			distance_event1_event2 <- distanceFunction(event1, points1, event2, points2, dmax, tmax, var_count, infra_count)
            
			distanceMatrix_spatiotemporal[i,j] <- distance_event1_event2["spatiotemporal"]
			distanceMatrix_socioeconomic[i,j] <- distance_event1_event2["socioeconomic"]
			distanceMatrix_infrastructure[i,j] <- distance_event1_event2["infrastructural"]
            
            distanceMatrix_spatial_km[i,j] <- distance_event1_event2["spatial_km"]
            distanceMatrix_spatial_normalized[i,j] <- distance_event1_event2["spatial_normalized"]
            distanceMatrix_temporal[i,j] <- distance_event1_event2["temporal_days"]
            distanceMatrix_temporal_normalized[i,j] <- distance_event1_event2["temporal_normalized"]
            distanceMatrix_spatial_weight[i,j] <- distance_event1_event2["wspatial"]
            distanceMatrix_temporal_weight[i,j] <- distance_event1_event2["wtemporal"]
			# print(distance_event1_event2)
		}
	}
	distanceMatrix_spatiotemporal <- distanceMatrix_spatiotemporal + t(distanceMatrix_spatiotemporal)
	distanceMatrix_socioeconomic <- distanceMatrix_socioeconomic + t(distanceMatrix_socioeconomic)
	distanceMatrix_infrastructure <- distanceMatrix_infrastructure + t(distanceMatrix_infrastructure)
    distanceMatrix_spatial_km <- distanceMatrix_spatial_km + t(distanceMatrix_spatial_km)
    distanceMatrix_spatial_normalized <- distanceMatrix_spatial_normalized + t(distanceMatrix_spatial_normalized)
    distanceMatrix_temporal <- distanceMatrix_temporal + t(distanceMatrix_temporal)
    distanceMatrix_temporal_normalized <- distanceMatrix_temporal_normalized + t(distanceMatrix_temporal_normalized)
    distanceMatrix_spatial_weight <- distanceMatrix_spatial_weight + t(distanceMatrix_spatial_weight)
    distanceMatrix_temporal_weight <- distanceMatrix_temporal_weight + t(distanceMatrix_temporal_weight)
	
    write.csv(distanceMatrix_spatiotemporal, "distanceMatrix_spatiotemporal.csv")
    write.csv(distanceMatrix_socioeconomic, "distanceMatrix_socioeconomic.csv")
    write.csv(distanceMatrix_infrastructure, "distanceMatrix_infrastructure.csv")
    write.csv(distanceMatrix_spatial_km, "distanceMatrix_spatial_km.csv")
    write.csv(distanceMatrix_spatial_normalized, "distanceMatrix_spatial_normalized.csv")
    write.csv(distanceMatrix_temporal, "distanceMatrix_temporal.csv")
    write.csv(distanceMatrix_temporal_normalized, "distanceMatrix_temporal_normalized.csv")
    write.csv(distanceMatrix_spatial_weight, "distanceMatrix_spatial_weight.csv")
    write.csv(distanceMatrix_temporal_weight, "distanceMatrix_temporal_weight.csv")
    
    
    #   distanceMatrix_spatiotemporal_recent <<- distanceMatrix_spatiotemporal
    #   distanceMatrix_socioeconomic_recent <<- distanceMatrix_socioeconomic
    #   distanceMatrix_infrastructure_recent <<- distanceMatrix_infrastructure
    #   assign(paste("distanceMatrix_spatiotemporal",Sys.Date(),sep="_"), distanceMatrix_spatiotemporal_recent)
    #   assign(paste("distanceMatrix_socioeconomic",Sys.Date(),sep="_"), distanceMatrix_socioeconomic_recent)
    #   assign(paste("distanceMatrix_infrastructure",Sys.Date(),sep="_"),  distanceMatrix_infrastructure_recent)

	if(spatialORsocioORinfra=='spatiotemporal'){
		distanceMatrix_final <- distanceMatrix_spatiotemporal
	} else if(spatialORsocioORinfra=='socioeconomic'){
		distanceMatrix_final <- distanceMatrix_socioeconomic
	} else if(spatialORsocioORinfra == 'infrastructure'){
		distanceMatrix_final <- distanceMatrix_infrastructure
	} else if(spatialORsocioORinfra == 'spatiotemporal_infrastructure'){
		distanceMatrix_final <- (distanceMatrix_spatiotemporal * weight_spatiotemporal) + (weight_infrastructure * distanceMatrix_infrastructure)
    } else if(spatialORsocioORinfra == 'spatiotemporal_socioeconomic'){
        distanceMatrix_final <- (distanceMatrix_spatiotemporal * weight_spatiotemporal) + (weight_socioeconomic * distanceMatrix_socioeconomic)
    } else if(spatialORsocioORinfra == 'all'){
		weight_infrastructure <- 1 - (weight_spatiotemporal + weight_socioeconomic)
        # print(paste("weight_infrastructure",weight_infrastructure))
		distanceMatrix_final <- (distanceMatrix_spatiotemporal * weight_spatiotemporal) + (weight_socioeconomic * distanceMatrix_socioeconomic) + (weight_infrastructure * distanceMatrix_infrastructure)
	} else {
		stop("weights and spatialORsocioORinfra do not make sense.")
	}
	# distanceMatrix_final <- distanceMatrix_final+t(distanceMatrix_final)
    write.csv(distanceMatrix_final, "distanceMatrix_final.csv")
	return(distanceMatrix_final)
}
