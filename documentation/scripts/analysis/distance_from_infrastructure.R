poi_india_police <- read.csv("./shapefiles/poi/poi_india_police.csv", header = T)
poi_india_postal <- read.csv("./shapefiles/poi/poi_india_postal.csv", header = T)
poi_india_university <- read.csv("./shapefiles/poi/poi_india_university.csv", header = T)
poi_india_college <- read.csv("./shapefiles/poi/poi_india_college.csv", header = T)
poi_india_school <- read.csv("./shapefiles/poi/poi_india_school.csv", header = T)
poi_india_hospital <- read.csv("./shapefiles/poi/poi_india_hospital.csv", header = T)

# First I collect the matrix of distances of each event with all the different kinds of infrastructures
# I create one matrix for each type of infrastructure.
# during the calculations, I also log the minimum distance from the calculated distances for each event. This information is in the 2nd column of the matrix.
# I also log the number of infrastructure elements within a distance of 10 km (10000 m). This information is in the 3rd column of the matrix.
latlon_event_india_2014 <- sqldf::sqldf("select distinct lat, lon from events;")
latlon_event_india_2014$uniqueid <- rownames(latlon_event_india_2014)
n <- nrow(latlon_event_india_2014)
#------------------------------------------
getInfrastructureDistance <- function(x, poi, neighborhood_km = 10000.00){
    n <- nrow(x)
    m <- nrow(poi)
    m_poi <- matrix( 0, ncol = 3, nrow = n)
    points1 <- as.matrix(x[,c("lon", "lat")])
    points2 <- as.matrix(poi[,c("lon", "lat")])
    for(i in 1:n){
        m_poi[i,1] <- x[i,"uniqueid"]
        count <- 0
        min_distance <- +Inf
        for(j in 1:m){
            d <- as.numeric(distHaversine(points1[i,], points2[j,]))
            if (d <= neighborhood_km){count <- count+1}
            if (d < min_distance) {min_distance = d}
        }
        m_poi[i,2] <- min_distance
        m_poi[i,3] <- count
    }
    return(m_poi)
}
#-------------------------------------------
m_police_distance <- getInfrastructureDistance(x = latlon_event_india_2014, poi = poi_india_police, neighborhood_km = 10000.00)
m_postal_distance <- getInfrastructureDistance(x = latlon_event_india_2014, poi = poi_india_postal, neighborhood_km = 10000.00)
m_hospital_distance <- getInfrastructureDistance(x = latlon_event_india_2014, poi = poi_india_hospital, neighborhood_km = 10000.00)
m_university_distance <- getInfrastructureDistance(x = latlon_event_india_2014, poi = poi_india_university, neighborhood_km = 10000.00)
m_college_distance <- getInfrastructureDistance(x = latlon_event_india_2014, poi = poi_india_college, neighborhood_km = 10000.00)
m_school_distance <- getInfrastructureDistance(x = latlon_event_india_2014, poi = poi_india_school, neighborhood_km = 10000.00)
#-------------------------------------------
# Now for the ease of calculation, we create a new matrix (and then data.frame) to store the summary of the information above.
##m_infrastructure_density_summary <- matrix( 0, ncol = 7, nrow = n)

m_infrastructure_proximity_summary <- data.frame(uniqueid=character(),
police_proximity = numeric(),
postal_proximity = numeric(),
hospital_proximity = numeric(),
university_proximity = numeric(),
college_proximity = numeric(),
school_proximity = numeric(),
stringsAsFactors=FALSE )

m_infrastructure_density_summary <- data.frame(uniqueid=character(),
police_density = numeric(),
postal_density = numeric(),
hospital_density = numeric(),
university_density = numeric(),
college_density = numeric(),
school_density = numeric(),
stringsAsFactors=FALSE )


##m_infrastructure_proximity_summary <- matrix( 0, ncol = 7, nrow = n)
## colnames(m_infrastructure_proximity_summary) <- c( "uniqueid", "police_proximity","postal_proximity","hospital_proximity","university_proximity","college_proximity","school_proximity")
## colnames(m_infrastructure_density_summary) <- c( "uniqueid", "police_density","postal_density","hospital_density","university_density","college_density","school_density")
for (i in 1:n){
    uq_id <- as.numeric(latlon_event_india_2014[i,"uniqueid"])
    m_infrastructure_proximity_summary[i,1] <- uq_id
    m_infrastructure_proximity_summary[i,2] <- as.numeric(m_police_distance[m_police_distance[,1] == uq_id, 2])
    m_infrastructure_proximity_summary[i,3] <- as.numeric(m_postal_distance[m_postal_distance[,1] == uq_id, 2])
    m_infrastructure_proximity_summary[i,4] <- as.numeric(m_hospital_distance[m_hospital_distance[,1] == uq_id, 2])
    m_infrastructure_proximity_summary[i,5] <- as.numeric(m_university_distance[m_university_distance[,1] == uq_id, 2])
    m_infrastructure_proximity_summary[i,6] <- as.numeric(m_college_distance[m_college_distance[,1] == uq_id, 2])
    m_infrastructure_proximity_summary[i,7] <- as.numeric(m_school_distance[m_school_distance[,1] == uq_id, 2])
    m_infrastructure_density_summary[i,1] <- uq_id
    m_infrastructure_density_summary[i,2] <- as.numeric(m_police_distance[m_police_distance[,1] == uq_id, 3])
    m_infrastructure_density_summary[i,3] <- as.numeric(m_postal_distance[m_postal_distance[,1] == uq_id, 3])
    m_infrastructure_density_summary[i,4] <- as.numeric(m_hospital_distance[m_hospital_distance[,1] == uq_id, 3])
    m_infrastructure_density_summary[i,5] <- as.numeric(m_university_distance[m_university_distance[,1] == uq_id, 3])
    m_infrastructure_density_summary[i,6] <- as.numeric(m_college_distance[m_college_distance[,1] == uq_id, 3])
    m_infrastructure_density_summary[i,7] <- as.numeric(m_school_distance[m_school_distance[,1] == uq_id, 3])
}
#--------------------------------------------
#m_infrastructure_proximity_summary <- as.data.frame(m_infrastructure_proximity_summary)
#m_infrastructure_density_summary <- as.data.frame(m_infrastructure_density_summary)

# Now, finally we normalize the data using feature scaling.
m_infrastructure_density_summary_normalized <- m_infrastructure_density_summary
m_infrastructure_proximity_summary_normalized <- m_infrastructure_proximity_summary

write.csv(m_infrastructure_density_summary,"m_infrastructure_density_summary.csv")
write.csv(m_infrastructure_proximity_summary,"m_infrastructure_proximity_summary.csv")
for (i in 2:7){
    minimum <- min(m_infrastructure_density_summary_normalized[,i])
    maximum <- max(m_infrastructure_density_summary_normalized[,i])
    m_infrastructure_density_summary_normalized[,i] <- (m_infrastructure_density_summary_normalized[,i] - minimum)/(maximum - minimum)
    minimum_ <- min(m_infrastructure_proximity_summary_normalized[,i])
    maximum_ <- max(m_infrastructure_proximity_summary_normalized[,i])
    m_infrastructure_proximity_summary_normalized[,i] <- (m_infrastructure_proximity_summary_normalized[,i] - minimum_)/(maximum_ - minimum_)
}
#---------------------------------------------
latlon_event_india_2014 <- merge(latlon_event_india_2014, m_infrastructure_proximity_summary_normalized, by.x = "uniqueid", by.y = "uniqueid", all = TRUE)
latlon_event_india_2014 <- merge(latlon_event_india_2014, m_infrastructure_density_summary_normalized, by.x = "uniqueid", by.y = "uniqueid", all = TRUE)
latlon_event_india_2014 <- latlon_event_india_2014[,c(2:15)]
events <- merge(events, latlon_event_india_2014, by.x = c("lat","lon"), by.y = c("lat","lon"), all = TRUE )
# write.csv(events,"events_india_2014.csv")
