distanceFunction_spatial <- function(eventData, dmax, tmax, var_count = 0, fun=distHaversine, weight_spatiotemporal =1) {
	if (weight_spatiotemporal > 1 || weight_spatiotemporal < 0){
		stop("weights for spatiotemporal must be between 0 and 1")
	}
	weight_socioeconomic <- 1 - weight_spatiotemporal

	.spatio <- function(eventData, dmax, tmax, fun=distHaversine){
		# A is Area with population and population density defined
		# A is a shapefile
		points <- .pointsToMatrix(eventData)
		n = nrow(points)
		distanceMatrix_spatiotemporal <- matrix(0, ncol=n, nrow=n)
		distanceMatrix_spatial <- matrix(0, ncol=n, nrow=n)
		distanceMatrix_spatial_actual <- matrix(0, ncol=n, nrow=n)
		distanceMatrix_temporal <- matrix(0, ncol=n, nrow=n)
		distanceMatrix_confidence_weight_spatial <- matrix(0, ncol=n, nrow=n)
		distanceMatrix_confidence_weight_temporal <- matrix(0, ncol=n, nrow=n)
		if (n == 1) {	
			return(distanceMatrix_spatiotemporal) 	
		}
		
		for (i in 2:n) {
			for (j in 1:(i-1)){
			dspatial <- fun(points[i,], points[j,]) # default function is Haversine
			dspatial <- round(dspatial/1000 , 4) # rounding off to 4 decimal digits after turning meters to km
			# print(paste("dspatial > dmax", dspatial ,dmax))
			distanceMatrix_spatial_actual[i,j] = dspatial
			dspatialnormalized <- ifelse((dspatial > dmax), 1, round(dspatial/dmax,4)) # if the distance is greater than the max distance specified, the normalized distance is the max distance itself

			# choosing the main event and the secondary event
			main <- ifelse(eventData$event_start[i] < eventData$event_start[j], 'event1 i', 
							ifelse(eventData$event_start[i] > eventData$event_start[j], 'event2 j', 
								ifelse(as.numeric(eventData$p[i]) < as.numeric(eventData$p[j]),'event2 j','event1 i')))
			if (main == 'event1 i') {
				mainEvent <- i
				secondEvent <- j
			} else {
				secondEvent <- i
				mainEvent <- j
			}

			pop <- round(eventData$p[mainEvent]/(eventData$p[mainEvent] + eventData$p[secondEvent]),4)
			# print(paste("pop: ",pop))

			# temporal directional distance
			dtemporal_directional <- as.vector(ifelse(eventData$event_start[mainEvent] == eventData$event_end[mainEvent], 
							eventData$event_start[secondEvent] - eventData$event_end[mainEvent],
								(eventData$event_start[secondEvent] - eventData$event_end[mainEvent])/(eventData$event_end[mainEvent] - eventData$event_start[mainEvent])))
			# normalized the above distance, if distance greater than threshold, then 1 (max)
			dtemporalnormalized <- ifelse((dtemporal_directional > tmax), 1, round(dtemporal_directional/tmax,4))
	    	#print(paste("dtemporal_directional: ",dtemporal_directional," dspatialnormalized: ",dspatialnormalized))
			#weights based on the population density of  the mainEvent
			wtemporal <- pop #popden #popdensity
			wspatial <- 1 - pop # 1- popden #popdensity 
			# final spatio-temporal weight
	    	# print(paste(dtemporalnormalized,  wtemporal, dspatialnormalized, wspatial))
			# print(paste("dtemporalnormalized ",dtemporalnormalized, "dspatialnormalized ", dspatialnormalized))
			# print(paste("wtemporal ",wtemporal, "wspatial ", wspatial))
			distanceMatrix_spatiotemporal[i,j] <- (dtemporalnormalized * wtemporal) + (dspatialnormalized * wspatial)
			distanceMatrix_spatial[i,j] <- dspatialnormalized
			distanceMatrix_temporal[i,j] <- dtemporalnormalized
			distanceMatrix_confidence_weight_spatial[i,j] <- wspatial
			distanceMatrix_confidence_weight_temporal[i,j] <- wtemporal
			}
		}
		distanceMatrix_spatiotemporal <<- distanceMatrix_spatiotemporal+t(distanceMatrix_spatiotemporal)
		distanceMatrix_spatial<<- distanceMatrix_spatial + t(distanceMatrix_spatial)
		distanceMatrix_temporal <<- distanceMatrix_temporal + t(distanceMatrix_temporal)
		distanceMatrix_confidence_weight_spatial<<- distanceMatrix_confidence_weight_spatial + t(distanceMatrix_confidence_weight_spatial)
		distanceMatrix_confidence_weight_temporal<<- distanceMatrix_confidence_weight_temporal + t(distanceMatrix_confidence_weight_temporal)
		# write.xlsx(distanceMatrix_spatial, "distanceMatrix_spatial.xlsx")
		# write.xlsx(distanceMatrix_temporal, "distanceMatrix_temporal.xlsx")
		# write.xlsx(distanceMatrix_confidence_weight_spatial, "distanceMatrix_confidence_weight_spatial.xlsx")
		# write.xlsx(distanceMatrix_confidence_weight_temporal, "distanceMatrix_confidence_weight_temporal.xlsx")
		# write.xlsx(distanceMatrix_spatiotemporal, "distanceMatrix_spatiotemporal.xlsx")
		# return(distanceMatrix_spatiotemporal)
		return(distanceMatrix_spatial)
	}
	d_spatiotemporal <- .spatio(eventData, dmax, tmax, fun=distHaversine)
	# write.xlsx(d_spatiotemporal, "donotuse_d_spatiotemporal.xlsx")
	d_final <- (weight_socioeconomic * d_socioeconomic) + (weight_spatiotemporal * d_spatiotemporal)
	distanceMatrix_final_combined <<- d_final
	# write.xlsx(d_final, "distanceMatrix_final.xlsx")
	# return(d_final)
	return(d_spatiotemporal)
}