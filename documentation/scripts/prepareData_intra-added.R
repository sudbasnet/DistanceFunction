prepareData_intra <- function(eventData, A, var = NA, infra_proximity = NA, infra_density = NA){
	if (is.na(var)) {
		m1 <- A
	} else {
		m1 <- merge(A, var, by.x ="id" , by.y="areacode", all= TRUE)
		if (!is.na(infra_proximity)) {
			m1 <- spCbind(m1,infra_proximity)
			m1 <- spCbind(m1,infra_density)
		}
	}
		m2 <- raster::intersect(eventData, m1)
		m3 <- merge(eventData, m2@data, all=TRUE)
		return (m3)
	}