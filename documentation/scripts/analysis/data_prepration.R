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

prepareArea <- function(x, id, parentid, description, population, area, resolution){
# parentid is the id of any area that includes the current area
  if(class(x) != 'SpatialPolygonsDataFrame') stop("Area should be a Polygon")
  popden <- round(x@data[population]/x@data[area],4)
  names(popden) <- c("pd")
  x$pd <-popden$pd
  A <- x[,c(id, parentid, description, population, area, "pd")]
  A$spatialres <- resolution
  names(A@data) <- c("id","parentid", "description", "p", "area_km_sq", "pd", "spatialres")
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

prepareData <- function(eventData, A, var = NA){
	if (is.na(var)) {
		m1 <- A
	} else {
		m1 <- merge(A, var, by.x ="id" , by.y="areacode", all= TRUE)
	}
		m2 <- raster::intersect(eventData, m1)
		m3 <- merge(eventData, m2@data, all=TRUE)
		return (m3)
	}