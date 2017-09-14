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