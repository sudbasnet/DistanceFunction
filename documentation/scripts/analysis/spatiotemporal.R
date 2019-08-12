	.spatiotemporal <- function(eventData, dmax, tmax, fun=distHaversine, A){
		# A is Area with population and population density defined
		# A is a shapefile 
		# print(head(eventData))
		eventData <- raster::intersect(eventData, A)
		# print(head(eventData))
		# eventData <- dplyr::left_join(eventData, eventData_, by = "uniqueid")
		# print(head(eventData))
	  # print(head(eventData))
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
		# normalizing the population density
		# Xmax = max(eventData$pd) 
		# #print(paste("Xmax: ", Xmax))
		# Xmin = min(eventData$pd)
		# #print(paste("Xmin: ", Xmin))
		# Xdenom = Xmax - Xmin
		# eventData$pd.normalized <- round((eventData$pd - Xmin)/Xdenom , 2) #normalized population density
		
		for (i in 2:n) {
			for (j in 1:(i-1)){
			dspatial <- fun(points[i,], points[j,]) # default function is Haversine
			dspatial <- round(dspatial/1000 , 2) # rounding off to 4 decimal digits after turning meters to km
			# print(paste("dspatial > dmax", dspatial ,dmax))
			distanceMatrix_spatial_actual[i,j] = dspatial
			dspatialnormalized <- ifelse((dspatial > dmax), 1, round(dspatial/dmax,2)) # if the distance is greater than the max distance specified, the normalized distance is the max distance itself

			# choosing the main event and the secondary event
			mainEvent <- ifelse(eventData$event_start[i] < eventData$event_start[j], i, 
							ifelse(eventData$event_start[i] > eventData$event_start[j], j, 
								ifelse(eventData$p[i] < eventData$p[j],j,i)))
			secondEvent <- ifelse(eventData$event_start[i] < eventData$event_start[j], j, 
							ifelse(eventData$event_start[i] > eventData$event_start[j], i, 
								ifelse(eventData$pd[i] < eventData$pd[j],i,j)))
			popdensity <- round(eventData$p[mainEvent]/(eventData$p[mainEvent] + eventData$p[secondEvent]),4)
			popden <- eventData$pd.normalized[mainEvent]
			# print(paste("mainEvent: ", mainEvent, "secondEvent: ", secondEvent))
			pop <- round(eventData$p[mainEvent]/(eventData$p[mainEvent] + eventData$p[secondEvent]),4)
			# ifelse()
			# print(paste("pop: ",pop))

			# temporal directional distance
			dtemporal_directional <- as.vector(ifelse(eventData$event_start[mainEvent] == eventData$event_end[mainEvent], 
							eventData$event_start[secondEvent] - eventData$event_end[mainEvent],
								(eventData$event_start[secondEvent] - eventData$event_end[mainEvent])/(eventData$event_end[mainEvent] - eventData$event_start[mainEvent])))
			# normalized the above distance, if distance greater than threshold, then 1 (max)
			dtemporalnormalized <- ifelse((dtemporal_directional > tmax), 1, round(dtemporal_directional/tmax,2))
	    	#print(paste("dtemporal_directional: ",dtemporal_directional," dspatialnormalized: ",dspatialnormalized))
			#weights based on the population density of  the mainEvent
			wtemporal <- pop #popden #popdensity
			wspatial <- 1 - pop # 1- popden #popdensity 
			# final spatio-temporal weight
	    	# print(paste(dtemporalnormalized,  wtemporal, dspatialnormalized, wspatial))
			
			distanceMatrix_spatiotemporal[i,j] <- (dtemporalnormalized * wtemporal) + (dspatialnormalized * wspatial)
			distanceMatrix_spatial[i,j] <- dspatialnormalized
			distanceMatrix_temporal[i,j] <- dtemporalnormalized
			distanceMatrix_confidence_weight_spatial[i,j] <- wspatial
			distanceMatrix_confidence_weight_temporal[i,j] <- wtemporal
			}
		}
		distanceMatrix_spatiotemporal <- distanceMatrix_spatiotemporal+t(distanceMatrix_spatiotemporal)
		distanceMatrix_spatial<- distanceMatrix_spatial + t(distanceMatrix_spatial)
		distanceMatrix_temporal <- distanceMatrix_temporal + t(distanceMatrix_temporal)
		distanceMatrix_confidence_weight_spatial<- distanceMatrix_confidence_weight_spatial + t(distanceMatrix_confidence_weight_spatial)
		distanceMatrix_confidence_weight_temporal<- distanceMatrix_confidence_weight_temporal + t(distanceMatrix_confidence_weight_temporal)
		write.xlsx(distanceMatrix_spatial, "distanceMatrix_spatial.xlsx")
		write.xlsx(distanceMatrix_temporal, "distanceMatrix_temporal.xlsx")
		write.xlsx(distanceMatrix_confidence_weight_spatial, "distanceMatrix_confidence_weight_spatial.xlsx")
		write.xlsx(distanceMatrix_confidence_weight_temporal, "distanceMatrix_confidence_weight_temporal.xlsx")
		write.xlsx(distanceMatrix_spatiotemporal, "distanceMatrix_spatiotemporal.xlsx")
		return(distanceMatrix_spatiotemporal)
	}