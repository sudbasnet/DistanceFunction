prepareEvent <- function(x, lat, lon, tstart, tend, eventCategory, url, uniqueid = NA){
	if(class(x) == 'SpatialPointsDataFrame') eventData <- data.frame(x@coords[,c(lat,lon)], x@data[,c(tstart, tend, eventCategory, url)])
	else eventData <- data.frame(x[,c(lat, lon, tstart, tend, eventCategory, url)])
	names(eventData) <- c("lat","lon", "event_start", "event_end", "eventCategory", "url")
	if (is.na(uniqueid)) {
		eventData$uniqueid <- seq.int(nrow(eventData))
		eventData <- eventData[,c("uniqueid","lat","lon", "event_start", "event_end", "eventCategory", "url")]
	} else {
		eventData$uniqueid <- x[,c(uniqueid)]
		eventData <- eventData[,c("uniqueid","lat","lon", "event_start", "event_end", "eventCategory", "url")]
	}
	coordinates(eventData)=~lon+lat
	crs(eventData)="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
	return(eventData)
}