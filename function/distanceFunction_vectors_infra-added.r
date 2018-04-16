distanceFunction_vectors_infra_working <-
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
