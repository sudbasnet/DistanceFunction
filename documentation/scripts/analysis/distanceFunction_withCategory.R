library("assertthat","geosphere")

m_unrest <- rbind(c(0,1,2,3,4,5,6,7),c(1,0,1,2,3,4,5,6),c(2,1,0,1,2,3,4,5),c(3,2,1,0,1,2,3,4),c(4,3,2,1,0,1,2,3),c(5,4,3,2,1,0,1,2),c(6,5,4,3,2,1,0,1),c(7,6,5,4,3,2,1,0))
unrest_names <- list('Appeal','Demand','Threaten','Protest','Coerce','Assault','Fight','Engage in UMV')
rownames(m_unrest) <- unrest_names
colnames(m_unrest) <- unrest_names

.spatiotemporal <- function(eventData, dmax, tmax, fun=distHaversine) {
	
	n = nrow(points)
	dm = matrix(0, ncol=n, nrow=n)
	if (n == 1) {	
		return(dm) 	
	}
	for (i in 2:n) {
		for (j in 1:(i-1)){
		dspatial = fun(points[i,], points[j,]) # default function is Haversine
		dspatial = round(dspatial/1000 , 2) # rounding off to 4 decimal digits
		print (paste("dspatial: ",dspatial))
		# dspatialnormalized = ifelse((dspatial > dmax), Inf, round(dspatial/dmax,2)) # if the distance is greater than the max distance specified, the normalized distance is Infinite(Inf)
		dspatialnormalized = ifelse((dspatial > dmax), 1, round(dspatial/dmax,2)) # if the distance is greater than the max distance specified, the normalized distance is the max distance itself

		print (paste("dspatial, dmax, dspatial/dmax: ",dspatial, dmax, dspatialnormalized))
		Xmax = max(otherdata$popdensity)
		Xmin = min(otherdata$popdensity)
		Xdenom = Xmax - Xmin
		print (paste("Xmax, Xmin, Xdenom: ",Xmax, Xmin, Xdenom))
		otherdata$popden.normalized <- round((otherdata$popdensity - Xmin)/Xdenom , 2) #normalized population density

		popden = ifelse ( otherdata$t[i] < otherdata$t[j], otherdata$popden.normalized[i], ifelse( otherdata$t[i] > otherdata$t[j], otherdata$popden.normalized[j], max(otherdata$popden.normalized[i],otherdata$popden.normalized[j])))
		# the earlier 
		print (paste("otherdata$t[i],otherdata$popden.normalized[i], otherdata$t[j],otherdata$popden.normalized[j], popden: ",otherdata$t[i],otherdata$popden.normalized[i], otherdata$t[j],otherdata$popden.normalized[j], popden))
		dtemporal =  abs(as.vector(otherdata$t[i] - otherdata$t[j]))
		print(paste("abs(as.vector(otherdata$t[i] - otherdata$t[j])): ",dtemporal))

		# dtemporalnormalized = ifelse((dtemporal > tmax), Inf, round(dtemporal/tmax,2))
		dtemporalnormalized = ifelse((dtemporal > tmax), 1, round(dtemporal/tmax,2))
		wtemporal = popden
		wspatial = 1- popden
		print(paste(dtemporalnormalized , " * " , wtemporal , " + " , dspatialnormalized ," * " ,wspatial))
		# dm[i,j] = ifelse(dtemporalnormalized == Inf || dspatialnormalized==Inf, Inf,  dtemporalnormalized * wtemporal + dspatialnormalized * wspatial)
		dm[i,j] = dtemporalnormalized * wtemporal + dspatialnormalized * wspatial
		print (paste("final distance: ",dm[i,j]))
		}
	}
	dm <- dm+t(dm)
	return(dm)
}

.thematic <- function(points, otherdata, fun) {
	n = nrow(points)
	dmt = matrix(0, ncol=n, nrow=n)
	if (n == 1) {	
		return(dmt) 	
	}
	for (i in 2:n) {
		for (j in 1:(i-1)){
		
		Xmax = max(otherdata$popdensity)
		Xmin = min(otherdata$popdensity)
		Xdenom = Xmax - Xmin
		print (paste("Xmax, Xmin, Xdenom: ",Xmax, Xmin, Xdenom))
		otherdata$popden.normalized <- round((otherdata$popdensity - Xmin)/Xdenom , 4) #normalized population density

		first = ifelse(otherdata$t[i] < otherdata$t[j], i, ifelse(otherdata$t[i] > otherdata$t[j], j ,ifelse(otherdata$popden.normalized[i] > otherdata$popden.normalized[j], i, j)))
		print(paste("otherdata$t[i] < otherdata$t[j]: ", otherdata$t[i] < otherdata$t[j]))
		print(paste("otherdata$t[i] > otherdata$t[j]: ", otherdata$t[i] > otherdata$t[j]))
		print(paste("otherdata$popden.normalized[i] > otherdata$popden.normalized[j]: ", otherdata$popden.normalized[i] > otherdata$popden.normalized[j]))
		print(paste("first: ", first))
		if(first==i) {second=j} else {second=i}
		print(paste("second: ", second))

		unrest_from = otherdata$eventCategory[first]
		unrest_to = otherdata$eventCategory[second]
		print(paste("first: ", unrest_from,", second: ",unrest_to))
		# thematic_distance = round(m_unrest[unrest_from, unrest_to]/7,4)
		dmt[i,j]=round(m_unrest[unrest_from, unrest_to]/7,4)
		}
	}
	dmt <- dmt+t(dmt)
	return(dmt)
}

prepareEvent <- function(x, lat, lon, tstart, tend, eventCategory, url){
	if(typeof(x) =='S4') eventData <- data.frame(x@coords[,c(lat,lon)], x@data[,c(tstart, tend, eventCategory, url)])
	else eventData <- data.frame(x[,c(lat, lon, tstart, tend, eventCategory, url)])
	names(eventData) <- c("lat","lon", "event_start", "event_end", "eventCategory", "url")
	coordinates(eventData)=~lon+lat
	return(eventData)

unrestDistance <- function(x, dmax, tmax, fun=distHaversine){
	# here x is the data.frame that has been passed through the prepareEvent function

	## first check if the parameters are correct
    # if(!is.data.frame(data)) stop("Data needs to be a data frame.")
    # pd <- x@data[,popdensity]
    # if(any(is.na(pd))) stop("Unknown values for population density.")
    # timevalues <- x@data[,t]
    # if(any(is.date(timevalues))) stop("Invalid date values.")
    # needs library geosphere
    points <- .pointsToMatrix(x)
    print (paste(x@data[,t], x@data[,popdensity], x@data[,eventCategory]))
	otherdata <- data.frame(x@data[,t], x@data[,popdensity], x@data[,eventCategory])
	dmax <- as.vector(dmax)
	tmax <- as.vector(tmax)
	names(otherdata) <- c("t","popdensity", "eventCategory")
	print(otherdata)
	return( .spatiotemporal(points, otherdata, dmax, tmax, fun))
	# return( .thematic(points, otherdata))

}

# example: unrestDistance(x=distanceFunction, 'event_date', 'popDensity', 'event_type',dmax = 5000, tmax = 5)
