finalVariable <- function(eventData, A, var){
		m1 <- merge(A, var, by.x ="id" , by.y="areacode", all= TRUE)
		m2 <- raster::intersect(eventData, m1)
		m3 <- merge(eventData, m2@data, all=TRUE)
		return (m3)
	}