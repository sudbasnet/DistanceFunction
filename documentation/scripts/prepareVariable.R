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