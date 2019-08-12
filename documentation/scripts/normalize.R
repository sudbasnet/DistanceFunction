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