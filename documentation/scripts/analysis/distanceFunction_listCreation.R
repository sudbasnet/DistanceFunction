distanceFunction_using_vectors_infra_working_list <- function(
    
    m3, 
    dmax = 10000.00,
    tmax = 30,
    var_count = 0,
    infra_count = 0,
    fun = distHaversine,
    weight_spatiotemporal = 0.33,
    weight_socioeconomic = 0.33,
    spatialORsocioORinfra = 'all') {
    
    require("geosphere")
    
    if ((weight_spatiotemporal + weight_socioeconomic) > 1){
        stop(paste("weights are not correct, weight_spatiotemporal: ", weight_spatiotemporal, ", weight_socioeconomic: ", weight_socioeconomic, ".", sep=""))
    }
    
    prepareArguments <- function(events, rowNum, var_count, infra_count) {
        m <- ncol(events)
        # cols_not_variables <- which(colnames(events)=="spatialres.y")
        number_of_variables <- var_count
        number_of_infrastructures <- infra_count
        cols_not_variables <- m - (var_count + (2 * infra_count))
        # print(paste("number_of_variables: ",number_of_variables))
        event_length <- 4 + number_of_variables + (2 * number_of_infrastructures) # first 4 parameters are: population, event_startdate, event_enddate, category
        event_arg <- NA
        event_arg[1] <- events$p[rowNum]
        # print(events$p[rowNum])
        event_arg[2] <- as.character(events$event_start[rowNum],format="%Y-%m-%d")
        # print(as.character(events$event_start[rowNum],format="%Y-%m-%d"))
        event_arg[3] <- as.character(events$event_end[rowNum],format="%Y-%m-%d")
        # print(as.character(as.character(events$event_end[rowNum],format="%Y-%m-%d")))
        event_arg[4] <- events$eventCategory[rowNum]
        # print(events$eventCategory[rowNum])
        if (number_of_variables > 0 || number_of_infrastructures > 0) {
            for (x in 1:number_of_variables){
                ln_event <- length(event_arg)
                event_arg[ln_event + 1] <- as.vector(events[rowNum, (cols_not_variables + x)])
            }
            for (y in 1:number_of_infrastructures){ # this is for the proximity type infrastructure
                ln_event <- length(event_arg)
                event_arg[ln_event + 1] <- as.vector(events[rowNum, (cols_not_variables + number_of_variables + y)])
            }
            for (z in 1:number_of_infrastructures){ # this is for the density type infrastructure
                ln_event <- length(event_arg)
                event_arg[ln_event + 1] <- as.vector(events[rowNum, (cols_not_variables + number_of_variables + number_of_infrastructures + z)])
            }
        }
        return(event_arg)
    }
    
    # points <- .pointsToMatrix(m3)
    events <- m3
    n <- nrow(m3)
    m <- ncol(m3)
    distance_final <- list()
    
    for (i in 1:n) {
        distance_final[i] <- 0
        k <- 0
        for (j in 1:n){
            points1 <- as.matrix(m3[i,c("lon","lat")])
            points2 <- as.matrix(m3[j,c("lon","lat")])
            event1 <- prepareArguments(events, i, var_count = 2, infra_count = 6)
            event2 <- prepareArguments(events, j, var_count = 2, infra_count = 6)
            
            if(geosphere::distHaversine(points1, points2) <= 100000 & abs(as.vector(as.Date(event1[2], format="%Y-%m-%d") - as.Date(event2[2], format="%Y-%m-%d"))) <= 60){
                k <- k + 1
                distance_final[[i]][k] <- j
            }
        }
        #print(i)
        #print(distance_final[[i]])    
    }
    capture.output(print(distance_final), file="distance_final.txt")
    return(distance_final)
}

events_data <- read.csv("./events_data_west.csv", header = T)
events_data <- events_data[!is.na(events_data$p),c(2:ncol(events_data))]
events_data$event_start <- as.character(as.Date(events_data$event_start, format = "%Y-%m-%d"), format="%Y-%m-%d")
events_data$event_end <- as.character(as.Date(events_data$event_end, format = "%Y-%m-%d"), format="%Y-%m-%d")
d_final_list<-distanceFunction_using_vectors_infra_working_list(m3 = events_data)
