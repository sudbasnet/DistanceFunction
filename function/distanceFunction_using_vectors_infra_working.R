distanceFunction_using_vectors_infra_working <- function(
       
        m3, 
        dmax = 10000.00,
        tmax = 30,
        var_count = 0,
        infra_count = 0,
        fun = distHaversine,
        weight_spatiotemporal = 0.33,
        weight_socioeconomic = 0.33,
        spatialORsocioORinfra = 'all') {
        
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
        distanceMatrix_spatial_km <- matrix(0, ncol=n, nrow=n)
        distanceMatrix_temporal <- matrix(0, ncol=n, nrow=n)
        distanceMatrix_spatial_weight <- matrix(0, ncol=n, nrow=n)
        distanceMatrix_temporal_weight <- matrix(0, ncol=n, nrow=n)
        distanceMatrix_spatiotemporal <- matrix(0, ncol=n, nrow=n)
        distanceMatrix_socioeconomic <- matrix(0, ncol=n, nrow=n)
        distanceMatrix_infrastructure <- matrix(0, ncol=n, nrow=n)
        distanceMatrix_spatial_normalized <- matrix(0, ncol=n, nrow=n)
        distanceMatrix_temporal_normalized <- matrix(0, ncol=n, nrow=n)
        # distanceMatrix_final <- matrix(0, ncol=n, nrow=n)
        
        for (i in 2:n) {
            for (j in 1:(i-1)){
                points1 <- as.matrix(m3[i,c("lon","lat")])
                points2 <- as.matrix(m3[j,c("lon","lat")])
                event1 <- prepareArguments(events, i, var_count = 2, infra_count = 6)
                event2 <- prepareArguments(events, j, var_count = 2, infra_count = 6)
                # print(paste("i,j",i,j))
                distance_event1_event2 <- distanceFunction_vectors_infra_working(event1, points1, event2, points2, dmax, tmax, var_count, infra_count)
                
                distanceMatrix_spatiotemporal[i,j] <- distance_event1_event2["spatiotemporal"]
                distanceMatrix_socioeconomic[i,j] <- distance_event1_event2["socioeconomic"]
                distanceMatrix_infrastructure[i,j] <- distance_event1_event2["infrastructural"]
                
                distanceMatrix_spatial_km[i,j] <- distance_event1_event2["spatial_km"]
                distanceMatrix_spatial_normalized[i,j] <- distance_event1_event2["spatial_normalized"]
                distanceMatrix_temporal[i,j] <- distance_event1_event2["temporal_days"]
                distanceMatrix_temporal_normalized[i,j] <- distance_event1_event2["temporal_normalized"]
                distanceMatrix_spatial_weight[i,j] <- distance_event1_event2["wspatial"]
                distanceMatrix_temporal_weight[i,j] <- distance_event1_event2["wtemporal"]
                # print(distance_event1_event2)
            }
        }
        distanceMatrix_spatiotemporal <- distanceMatrix_spatiotemporal + t(distanceMatrix_spatiotemporal)
        distanceMatrix_socioeconomic <- distanceMatrix_socioeconomic + t(distanceMatrix_socioeconomic)
        distanceMatrix_infrastructure <- distanceMatrix_infrastructure + t(distanceMatrix_infrastructure)
        distanceMatrix_spatial_km <- distanceMatrix_spatial_km + t(distanceMatrix_spatial_km)
        distanceMatrix_spatial_normalized <- distanceMatrix_spatial_normalized + t(distanceMatrix_spatial_normalized)
        distanceMatrix_temporal <- distanceMatrix_temporal + t(distanceMatrix_temporal)
        distanceMatrix_temporal_normalized <- distanceMatrix_temporal_normalized + t(distanceMatrix_temporal_normalized)
        distanceMatrix_spatial_weight <- distanceMatrix_spatial_weight + t(distanceMatrix_spatial_weight)
        distanceMatrix_temporal_weight <- distanceMatrix_temporal_weight + t(distanceMatrix_temporal_weight)
        
        write.csv(distanceMatrix_spatiotemporal, "distanceMatrix_spatiotemporal.csv")
        write.csv(distanceMatrix_socioeconomic, "distanceMatrix_socioeconomic.csv")
        write.csv(distanceMatrix_infrastructure, "distanceMatrix_infrastructure.csv")
        write.csv(distanceMatrix_spatial_km, "distanceMatrix_spatial_km.csv")
        write.csv(distanceMatrix_spatial_normalized, "distanceMatrix_spatial_normalized.csv")
        write.csv(distanceMatrix_temporal, "distanceMatrix_temporal.csv")
        write.csv(distanceMatrix_temporal_normalized, "distanceMatrix_temporal_normalized.csv")
        write.csv(distanceMatrix_spatial_weight, "distanceMatrix_spatial_weight.csv")
        write.csv(distanceMatrix_temporal_weight, "distanceMatrix_temporal_weight.csv")
        
        
        #   distanceMatrix_spatiotemporal_recent <<- distanceMatrix_spatiotemporal
        #   distanceMatrix_socioeconomic_recent <<- distanceMatrix_socioeconomic
        #   distanceMatrix_infrastructure_recent <<- distanceMatrix_infrastructure
        #   assign(paste("distanceMatrix_spatiotemporal",Sys.Date(),sep="_"), distanceMatrix_spatiotemporal_recent)
        #   assign(paste("distanceMatrix_socioeconomic",Sys.Date(),sep="_"), distanceMatrix_socioeconomic_recent)
        #   assign(paste("distanceMatrix_infrastructure",Sys.Date(),sep="_"),  distanceMatrix_infrastructure_recent)
        
        if(spatialORsocioORinfra=='spatiotemporal'){
            distanceMatrix_final <- distanceMatrix_spatiotemporal
        } else if(spatialORsocioORinfra=='socioeconomic'){
            distanceMatrix_final <- distanceMatrix_socioeconomic
        } else if(spatialORsocioORinfra == 'infrastructure'){
            distanceMatrix_final <- distanceMatrix_infrastructure
        } else if(spatialORsocioORinfra == 'spatiotemporal_infrastructure'){
            distanceMatrix_final <- (distanceMatrix_spatiotemporal * weight_spatiotemporal) + (weight_infrastructure * distanceMatrix_infrastructure)
        } else if(spatialORsocioORinfra == 'spatiotemporal_socioeconomic'){
            distanceMatrix_final <- (distanceMatrix_spatiotemporal * weight_spatiotemporal) + (weight_socioeconomic * distanceMatrix_socioeconomic)
        } else if(spatialORsocioORinfra == 'all'){
            weight_infrastructure <- 1 - (weight_spatiotemporal + weight_socioeconomic)
            # print(paste("weight_infrastructure",weight_infrastructure))
            distanceMatrix_final <- (distanceMatrix_spatiotemporal * weight_spatiotemporal) + (weight_socioeconomic * distanceMatrix_socioeconomic) + (weight_infrastructure * distanceMatrix_infrastructure)
        } else {
            stop("weights and spatialORsocioORinfra do not make sense.")
        }
        # distanceMatrix_final <- distanceMatrix_final+t(distanceMatrix_final)
        write.csv(distanceMatrix_final, "distanceMatrix_final.csv")
        return(distanceMatrix_final)
    }