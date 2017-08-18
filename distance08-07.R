library("assertthat","geosphere","raster","dplyr", "maptools","sp", "rgdal", "rgeos", "spatstat", "stringr", "sqldf")

m_unrest <- rbind(c(0,1,2,3,4,5,6,7),
				  c(1,0,1,2,3,4,5,6),
				  c(2,1,0,1,2,3,4,5),
				  c(3,2,1,0,1,2,3,4),
				  c(4,3,2,1,0,1,2,3),
				  c(5,4,3,2,1,0,1,2),
				  c(6,5,4,3,2,1,0,1),
				  c(7,6,5,4,3,2,1,0))

unrest_names <- list('Appeal','Demand','Threaten','Protest','Coerce','Assault','Fight','Engage in UMV')
rownames(m_unrest) <- unrest_names
colnames(m_unrest) <- unrest_names

normalize <- function(x){
	# if(x[1])
	# take in a column of data/ data frame with one column and 
	# return another dataframe of one column with nirmalized values
	mini = min(x[!is.na(x)])
	maxi = max(x[!is.na(x)])
	print(paste(maxi,", ", mini))
	denom = maxi - mini
	x$norm <- round((x - mini) / denom,4)
	return(x$norm)
}

normalize_col <- function(x, columnName){
  # if(x[1])
  # take in a column of data/ data frame with one column and 
  # return another dataframe of one column with nirmalized values
  mini = min(x[!is.na(x[,columnName])])
  print(mini)
  maxi = max(x[!is.na(x[,columnName])])
  print(maxi)
  print(paste(maxi,", ", mini))
  denom = maxi - mini
  for (i in 1:nrow(x)){
    x[i][columnName] <- round((x[i][columnName] - mini)/denom,4)
  }
  return(x)
}

normalize_scale<- function(x){
	# if(x[1])
	# take in a column of data/ data frame with one column and 
	# return another dataframe of one column with nirmalized values
	x$norm <- scale(x)
	return(x$norm)
}

prepareEvent <- function(x, lat, lon, tstart, tend, eventCategory, url){
	if(class(x) == 'SpatialPointsDataFrame') eventData <- data.frame(x@coords[,c(lat,lon)], x@data[,c(tstart, tend, eventCategory, url)])
	else eventData <- data.frame(x[,c(lat, lon, tstart, tend, eventCategory, url)])
	names(eventData) <- c("lat","lon", "event_start", "event_end", "eventCategory", "url")
	eventData$uniqueid <- seq.int(nrow(eventData))
	eventData <- eventData[,c("uniqueid","lat","lon", "event_start", "event_end", "eventCategory", "url")]
	coordinates(eventData)=c(lon,lat)
	crs(eventData)="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
	return(eventData)
}

prepareArea <- function(x, id, parentid, description, population, area, resolution){
# parentid is the id of any area that includes the current area
  if(class(x) != 'SpatialPolygonsDataFrame') stop("Area should be a Polygon")
  popden <- round(x@data[population]/x@data[area],4)
  names(popden) <- c("pd")
  x$pd <-popden$pd
  A <- x[,c(id, parentid, description, population, "pd")]
  A$spatialres <- resolution
  names(A@data) <- c("id","parentid", "description", "p", "pd", "spatialres")
  return(A)
}


prepareVariable <- function(x, areaCode, date_dataCollection, area_resolution, time_frame = "yearly", normalizedata = FALSE){
	# x is the dataframe
	# areaCode is a column in the data-frame and must be equal to one of the id in Area
	# date_dataCollection can be a column in the data.frame 'x' or a date ('YYYY/MM/DD'), (YYYY/MM) or (YYYY) formats only
		# the length of the string will be checked and then the date value will be checked
	# timeframe can be weekly, monthly, daily or yearly, default is yearly. 
	# timeframe can also be a column in the dataframe, if it is not one of the above, it's assumed to be a column and searched in the dataframe x
	# if timeframe is given as a string weekly, monthly, daily or yearly, then the whole data-frame is assumed to be in this data-frame.
	# each column is treated as a separate variable, but only if it contains numeric values. If any column contains non-numeric values, the column is rejected
	dateFormat = NA
	timeFormat = NA
	if(date_dataCollection %in% colnames(x)){
		dateFormat = "column"
		names(x)[names(x) == date_dataCollection] <- "surveydate"
		# print(dateFormat)
		if(any(is.na(x$surveydate) == TRUE)) stop(paste("Incorrect data found in column: ",time_frame))
		} else {
			if((str_count(date_dataCollection, '/') == 2) & (!is.na(as.Date(date_dataCollection, format="%Y/%m/%d")))){
				# print("str_count(date_dataCollection, '/') == 2")
				dateFormat = "day"
				x$surveydate <- date_dataCollection
			} else if((str_count(date_dataCollection, '/') == 1) & (!is.na(as.Date(paste(date_dataCollection,"/01",sep=""), format="%Y/%m/%d")))){
				dateFormat = "month"
				x$surveydate <- date_dataCollection
			} else if((str_count(date_dataCollection, '/') == 0) & (!is.na(as.Date(paste(date_dataCollection,"/01", sep=""), format="%Y/%d")))){
				dateFormat = "year"
				x$surveydate <- date_dataCollection
			}
		}
		if (is.na(dateFormat)) stop("date format is not correct, supply date_dataCollection")

	if(time_frame %in% colnames(x)) {
		timeFormat = "column"
		names(x)[names(x) == time_frame] <- "timeframe"
	} else if (any(time_frame == c("yearly","monthly","weekly","daily"))){
		timeFormat = time_frame
		x$timeframe <- time_frame
	}
	if(is.na(dateFormat)) stop("time_frame value is not valid")
	x$spatialres <- area_resolution
	varData <- data.frame(x[,c(areaCode,"surveydate", "timeframe","spatialres")])
	names(varData) <- c("areacode", "surveydate", "timeframe", "spatialres")
	numberofcolumns <- ncol(x)
	for(i in 1:numberofcolumns){
		colName <- names(x[i])
		if(!any(names(x[i])== c(areaCode, "surveydate", "timeframe", "spatialres"))) {
			if(any(is.na(x[colName]) == TRUE)) print(paste("warning: column ", colName, "has NA values"))
			if(normalizedata == "TRUE"){
		    varData[colName] <- normalize(x[colName])
			} else varData[colName] <- x[colName]
		}
	}
	return(varData)
}

.spatiotemporal <- function(eventData, dmax, tmax, fun=distHaversine, A){
	# A is Area with population and population density defined
	# A is a shapefile 
	# print(head(eventData))
	eventData <- raster::intersect(eventData, A)
	# print(head(eventData_))
	# eventData <- dplyr::left_join(eventData, eventData_, by = "uniqueid")
	# print(head(eventData))
  # print(head(eventData))
	points <- .pointsToMatrix(eventData)
	n = nrow(points)
	distanceMatrix_spatiotemporal = matrix(0, ncol=n, nrow=n)
	if (n == 1) {	
		return(distanceMatrix_spatiotemporal) 	
	}
	# normalizing the population density
	Xmax = max(eventData$pd) 
	#print(paste("Xmax: ", Xmax))
	Xmin = min(eventData$pd)
	#print(paste("Xmin: ", Xmin))
	Xdenom = Xmax - Xmin
	eventData$pd.normalized <- round((eventData$pd - Xmin)/Xdenom , 2) #normalized population density
	
	for (i in 2:n) {
		for (j in 1:(i-1)){
		dspatial = fun(points[i,], points[j,]) # default function is Haversine
		dspatial = round(dspatial/1000 , 2) # rounding off to 4 decimal digits after turning meters to km
		# print(paste("dspatial > dmax", dspatial ,dmax))
		dspatialnormalized = ifelse((dspatial > dmax), 1, round(dspatial/dmax,2)) # if the distance is greater than the max distance specified, the normalized distance is the max distance itself

		# choosing the main event and the secondary event
		mainEvent = ifelse(eventData$event_start[i] < eventData$event_start[j], i, 
						ifelse(eventData$event_start[i] > eventData$event_start[j], j, 
							ifelse(eventData$pd.normalized[i] < eventData$pd.normalized[j],j,i)))
		secondEvent = ifelse(eventData$event_start[i] < eventData$event_start[j], j, 
						ifelse(eventData$event_start[i] > eventData$event_start[j], i, 
							ifelse(eventData$pd.normalized[i] < eventData$pd.normalized[j],i,j)))
		popdensity = round(eventData$pd[mainEvent]/(eventData$pd[mainEvent] + eventData$pd[secondEvent]),4)
		popden = eventData$pd.normalized[mainEvent]
	#	print(paste("popden: ",popden))

		# temporal directional distance
		dtemporal_directional = as.vector(ifelse(eventData$event_start[mainEvent] == eventData$event_end[mainEvent], 
						eventData$event_start[secondEvent] - eventData$event_end[mainEvent],
							(eventData$event_start[secondEvent] - eventData$event_end[mainEvent])/(eventData$event_end[mainEvent] - eventData$event_start[mainEvent])))
		# normalized the above distance, if distance greater than threshold, then 1 (max)
		dtemporalnormalized = ifelse((dtemporal_directional > tmax), 1, round(dtemporal_directional/tmax,2))
    #print(paste("dtemporal_directional: ",dtemporal_directional," dspatialnormalized: ",dspatialnormalized))
		#weights based on the population density of  the mainEvent
		wtemporal = popdensity #popden
		wspatial = 1- popdensity #popden
		# final spatio-temporal weight
    # print(paste(dtemporalnormalized,  wtemporal, dspatialnormalized, wspatial))
		distanceMatrix_spatiotemporal[i,j] = dtemporalnormalized * wtemporal + dspatialnormalized * wspatial
		}
	}
	distanceMatrix_spatiotemporal <- distanceMatrix_spatiotemporal+t(distanceMatrix_spatiotemporal)
	return(distanceMatrix_spatiotemporal)
}

# combineEventsVariablesArea <- function(event, var, Area ){
# 	m1 <- merge(var, Area@data, by.x = "areacode", by.y = "areacode")
# 	m2 <- merge(m1, Area@data, by.x = "areacode", by.y = "areacode")
# }

.socioeconomic <- function(eventData, Area, var){
	m1 <- merge(Area, var, by.x ="id" , by.y="areacode", all= TRUE)
	# m2 <- merge(m1, eventData, by.x= , by.y= , all= TRUE)
	m2 <- raster::intersect(eventData, m1)
	m3 <- merge(eventData, m2, all=TRUE)
  print(head(m3))
	points <- .pointsToMatrix(m3)
	n = nrow(m3)

	if (n == 1) {	
		return(distanceMatrix_socioeconomic) 	
	}
	returnList = list()
	# only send variables that have passed from prepareVariable
	for (k in 18:ncol(m3)){
		distanceMatrix_socioeconomic = matrix(0, ncol=n, nrow=n)
		for (i in 2:n){
			for (j in 1:(i-1)){
			  #print(paste("m3@data[i,k]: ",m3@data[i,k],"m3@data[j,k]: ", m3@data[j,k]))
				distanceMatrix_socioeconomic[i,j] = abs(m3@data[i,k] - m3@data[j,k])
			}
		}
		distanceMatrix_socioeconomic <- distanceMatrix_socioeconomic+t(distanceMatrix_socioeconomic)
		listObjName <- names(m3[,k])
		returnList[[listObjName]] <- distanceMatrix_socioeconomic
	}
	return(returnList)
}

.thematic <- function(eventData) {
	n = nrow(points)
	points <- .pointsToMatrix(eventData)
	dmt = matrix(0, ncol=n, nrow=n)
	if (n == 1) {	
		return(dmt) 	
	}
	for (i in 2:n) {
		for (j in 1:(i-1)){
		
		Xmax = max(eventData$popdensity)
		Xmin = min(eventData$popdensity)
		Xdenom = Xmax - Xmin
		print (paste("Xmax, Xmin, Xdenom: ",Xmax, Xmin, Xdenom))
		eventData$popden.normalized <- round((eventData$popdensity - Xmin)/Xdenom , 4) #normalized population density

		first = ifelse(eventData$t[i] < eventData$t[j], i, ifelse(eventData$t[i] > eventData$t[j], j ,ifelse(eventData$popden.normalized[i] > eventData$popden.normalized[j], i, j)))
		print(paste("eventData$t[i] < eventData$t[j]: ", eventData$t[i] < eventData$t[j]))
		print(paste("eventData$t[i] > eventData$t[j]: ", eventData$t[i] > eventData$t[j]))
		print(paste("eventData$popden.normalized[i] > eventData$popden.normalized[j]: ", eventData$popden.normalized[i] > eventData$popden.normalized[j]))
		print(paste("first: ", first))
		if(first==i) {second=j} else {second=i}
		print(paste("second: ", second))

		unrest_from = eventData$eventCategory[first]
		unrest_to = eventData$eventCategory[second]
		print(paste("first: ", unrest_from,", second: ",unrest_to))
		# thematic_distance = round(m_unrest[unrest_from, unrest_to]/7,4)
		dmt[i,j]=round(m_unrest[unrest_from, unrest_to]/7,4)
		}
	}
	dmt <- dmt+t(dmt)
	return(dmt)
}

transformSpatial <- function(var, targetResolution_s, sourceArea, targetArea){
	# target resolution is like district, state etc.
	# targetArea A
	#m2 <- merge(targetArea@data, var, by.x='parentid' , by.y = 'areacode', all=TRUE)
	#print(m2)
  if (nrow(targetArea) > nrow(sourceArea)){
	  m2 <- merge(targetArea@data, var, by.x='parentid' , by.y = 'areacode', all=TRUE)
	  var_m <- m2[c(2,1, 7:ncol(m2))]
	  print(head(var))
	  names(var_m) <- c("parentid", names(var))
	  var <- var_m
	  print(var)
	  m1 <- merge(var, sourceArea@data, by.x ="areacode", by.y = "id", all=TRUE)
	  colnames(m1)[2] <- "parentid"
  } else {
    m1 <- merge(var, sourceArea@data, by.x ="areacode", by.y = "id", all=TRUE)
    colnames(m1)[7] <- "parentid"
  }
  print(m1)
  n <- nrow(m1)
	for (i in 1:n){
		for (j in 6:ncol(var)){
			# print(j)
			if (var[i,]$spatialres == targetResolution_s) var[i,j] <- var[i,j]
			if (var[i,]$spatialres != targetResolution_s){
				populationChild <- m1[i,]$p
				parentid_ <- m1[i,]$parentid
				var[i,]$areacode <- m1[i,]$parentid
				populationParent <- targetArea@data[which(targetArea$id==parentid_),]$p
			#	print(parentid_)
				print(paste("populationParent: ",populationParent,", populationChild: ", populationChild))
				confidence_weight_s <- ifelse(populationParent > populationChild, round(populationChild/populationParent,4), round(populationParent/populationChild,4))
			  print(var[i,j])
				var[i,j] <- round(confidence_weight_s * var[i,j],4)
				#var[i,]$areacode <- m1[i,]$parentid
			}
		}
		var[i,]$spatialres <- targetResolution_s
	}
	var <- sqldf('SELECT * FROM var GROUP BY areacode')
	return(var)
}

transformTemporal <- function(var, targetResolution_t){
	n <- nrow(var)
	TimeFrame <- data.frame(c(1, 7, 30, 365), c("daily", "weekly", "monthly", "yearly"))
	names(TimeFrame) <- c("days","temporalres")
	for (i in 1:n){
		for (j in 5:ncol(var)){
			if (var[i,]$timeframe != targetResolution_t){
				source_days <- TimeFrame[which(TimeFrame$temporalres==var[i,]$timeframe),]$days
				target_days <- TimeFrame[which(TimeFrame$temporalres==targetResolution_t),]$days
				# print(paste("source",source_days," , target:",target_days))
				confidence_weight_t <- ifelse(source_days > target_days, round(target_days/source_days,4), round(source_days/target_days,4))
				var[i,j] <- round(confidence_weight_t * var[i,j],4)
			}
		}
		var[i,]$timeframe <- targetResolution_t
	}
	return(var)
}

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
	eventData <- data.frame(x@data[,t], x@data[,popdensity], x@data[,eventCategory])
	dmax <- as.vector(dmax)
	tmax <- as.vector(tmax)
	names(eventData) <- c("t","popdensity", "eventCategory")
	print(eventData)
	return( .spatiotemporal(points, eventData, dmax, tmax, fun))
	# return( .thematic(points, eventData))
}

# example: unrestDistance(x=distanceFunction, 'event_date', 'popDensity', 'event_type',dmax = 5000, tmax = 5)
# prepare data
# eventData <- prepareEvent(distanceFunction, 'lat', 'lon', 'event_date', 'event_date', 'event_type', "DISTRICT")
# plot(A[A$id==54,])  to select part of the polygon
# events$lat <- as.numeric(gsub(" ", "",gsub(")","",substr(events$WKT,30,46), fixed=TRUE)))
# events$lon <- as.numeric(gsub(" ", "",substr(events$WKT,14,29)))
# coordinates(events) =~lon+lat
## punjabDetails <- read.csv("/Applications/XAMPP/xamppfiles/htdocs/importPCA/PCA-census-variables-India/individuals-exports/PCA-03-Punjab.CSV", sep=",", header=T)
## punjabDetails$State <- substr(punjabDetails$State, 2, 5)
## punjabDetails$District <- substr(punjabDetails$District, 2, 5)
## punjabdata <- punjabDetails[punjabDetails$Level=="DISTRICT" & punjabDetails$TRU=="Total",]
## punjabdistrict <- (punjabdata[1:8])
## testmerge <- merge(A2, punjabrajesh, by.x="censuscode", by.y="District",  all=TRUE)
# var <- read.csv("var1.csv",sep=",",header=T)
# var$literacyRate <- as.numeric(var$literacyRate)
# var$workerPop <- as.numeric(var$workerPop)
## var1 <- prepareVariable(var, "District","2017","yearly")
##
#varpunjab <- read.csv("var1_punjab.csv",header=T, sep=",")
#varpunjab$District <- as.numeric(substr(varpunjab$District, 2, 5))
# data was in format '036, '056 so took substring
#varpunjab$State <- as.numeric(substr(varpunjab$State, 2, 5))
# testing .socioeconomic(event, var1, A3)

##
# d_socioeconomic_district <- .socioeconomic(event, A, var)
# names(d_socioeconomic_district) # [1] "literacyRate" "workerPop" 
# d_socioeconomic_district_literacyRate_dist <- as.dist(d_socioeconomic_district[["literacyRate"]])
# d_socioeconomic_district_workerPop_dist <- as.dist(d_socioeconomic_district[["workerPop"]])
# d_spatiotemporal_district <- .spatiotemporal(event, dmax = 100, tmax = 30, A = A)
# d_spatiotemporal_district_dist <- as.dist(d_spatiotemporal_district)
# d_socioeconomic_district <- (0.5 *d_socioeconomic_district_literacyRate_dist) + (0.5*d_socioeconomic_district_workerPop_dist)
# d_final_district_0.5each <- (d_spatiotemporal_district_dist * 0.5)+ (d_socioeconomic_district * 0.5)
# dbscan is not giving good results, so I tried kmeans
# kmeans_district_0.33each <- kmeans(d_final_district_0.33each, 3)
# d_socioeconomic_state <- .socioeconomic(event, A_state, var_state)
# names(d_socioeconomic_state) # [1] "literacyRate" "workerPop" 
# d_spatiotemporal_state <- .spatiotemporal(event, dmax = 100, tmax = 30, A = A_state)
# d_socioeconomic_state_literacyRate_dist <- as.dist(d_socioeconomic_state[["literacyRate"]])
# d_socioeconomic_state_workerPop_dist <- as.dist(d_socioeconomic_state[["workerPop"]])
# d_spatiotemporal_state_dist <- as.dist(d_spatiotemporal_state)
# d_socioeconomic_state <- (d_socioeconomic_state_workerPop_dist*0.5)+(d_socioeconomic_state_literacyRate_dist*0.5)
# d_socioeconomic_state_dist <- as.dist(d_socioeconomic_state)
# d_final_state_0.5each <- (d_spatiotemporal_state_dist * 0.5)+ (d_socioeconomic_state_dist*0.5)
# kmeans_state_0.33each <- kmeans(d_final_state_0.33each, 3)

# jpeg("kmeans_state_0.33_punjabRajeshtan.jpg", 4000, 4000, res=300)
# plot(A_state)
# plot(events, col=events$kmeans_state, add=T)
# dev.off()

# text(events@coords, labels=events$id, cex= 0.7, col=events$kmeans_district, pos=4)


#dbscan_state_0.5 <- dbscan(d_final_state_0.5each, eps = 0.2, minPts = 3)