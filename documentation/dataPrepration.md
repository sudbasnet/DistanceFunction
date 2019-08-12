# DATA PREPRATION

First I load the libraries that we'll need for this analysis with the folowing `sapply`. These libraries were used in different places during the project. Some libraries may be skipped if found unnecessary.

```R
sapply (c("sp", "raster", "spatstat", "maptools", "plotrix", "scatterplot3d", "assertthat", "geosphere","dbscan", "rgdal", "rgeos", "spatstat"), library, character.only = TRUE)
```

### **_Load events data (GDELT data)_**

We import the events data pulled from GDELT's event database for the year 2014 for India. We have choosen this year as it was the election year of India's current PM, Narendra Modi. We expect to have a moderate number of events throughout the country in this year. Since we are using the census data from 2011, it is important to note that we are assuming these census varaible to be consistent enough that we can use it for 2014.

```R
events_india_GDELT_2014 <- read.csv("./data/events_india_GDELT_2014.csv",header=T, sep="," )
# dates are read as characters, so we convert it to Date
events_india_GDELT_2014$event_date <- as.Date(substr(events_india_GDELT_2014$event_date, 1, 10),format = "%Y-%m-%d")
```

Now that we have the events data imported in the format we have for the SURGE database, we need to transform it into a data.frame structure that the distance function can automatically work with. I have used the function [**`prepareEvent`**](https://github.com/sudbasnet/distanceFunction/blob/master/dataPrepration/prepareEvent.R) that is available in the repository "link". This will also make the events data into a spatial dataframe, we can then intersect this with the map data.

```R
events <- prepareEvent(events_india_GDELT_2014, lat = "latitude", lon = "longitude", tstart = "event_date", tend = "event_date", eventCategory = "event_category", url = "url", uniqueid = "event_id")
```

### **_Load map shapefiles_**

I have downloaded the shapefiles for India which contains the administrative boundaries in 2011, during the time of India's census collection. The boundaries have changed since then, we use the 2011 ones because it syncs with the census data. This data was collected by the [DataMeet](http://datameet.org/wiki/catalog) community, the shapefiles were downloaded from their github repository at: https://github.com/datameet/maps.

```R
# districts
map_india_districts <- shapefile("./shapefiles/india_districts/2011_Dist.shp")
## try plotting it
# plot(map_india_districts)
```

Other than the geo-spatial boundaries, the shapefile also contains the following information. `DISTRICT`: district's name, `ST_NM`: state's name, `ST_CEN_CD`: Id assigned to the state, `DT_CEN_CD` id assigned to district, and `censuscode`: code assigned to the district that syncs with the Indian census' district id.

### **_Load socioeconomic (Census) data_**

Next, I would like to assign the population and some socioeconomic data values from the census-2011 dataset of India made public by the Indian government. I'd like to assign these values to the district level data.

```R
census_abstract_india_2011 <- read.csv("./data/pca-full.csv", sep=",", header=T)
# only keep the data that is for TOTAL (=Urban+Rural) and only pick the columns I think we need
census_abstract_india_2011 <- census_abstract_india_2011[ (toupper(census_abstract_india_2011$TRU)=='TOTAL') & (toupper(census_abstract_india_2011$Level)=='DISTRICT'),c(1,2,4,6,8,11,20,29)]

names(census_abstract_india_2011) <- c("State", "District", "Name", "area_km_sq","population_total", "population_age0to6", "population_literate", "population_main_workforce")
```

From the census extract, we can make the socioeconomic variables. This part will require some manual steps. For the project, we were only using the literacy rates and the workforce rate for the districts in India. We get these variables out of the census data by dividing the population of literates and the working population by the population of ages 7 yrs and above.

```R
socioeconomic_data <- census_abstract_india_2014[,c("District", "area_km_sq", "population_total")]
names(socioeconomic_data) <- c("id", "area", "p")

socioeconomic_data$pd <- socioeconomic_data$population_total/socioeconomic_data$area_km_sq

socioeconomic_data$literacy_rate <- census_abstract_india_2014$population_literate/(census_abstract_india_2014$population_total -  census_abstract_india_2014$population_age0to6)

socioeconomic_data$employment_rate <- census_abstract_india_2014$population_main_workforce/(census_abstract_india_2014$population_total -  census_abstract_india_2014$population_age0to6)

```

### **_Combine `area` and `socioeconomic_data`_**

Now we use the `merge` function of R to pull the area and population values from the census dataset into the shapefiles. This is a really powerful function and perfect if u want to merge two datasets. As we can see, this function can actually combine a data of class SpatialObject and regular data.frame. We use the `censuscode` column from the shapefile and the `District` column in the census data to merge the two datasets.

```R
area <- merge(map_india_districts, socioeconomic_data, by.x="censuscode", by.y="id", all = FALSE)
# all = FALSE means we only keep the ones that merge
```

### **_Add the socioeconomic data to the main events data-set_**

Now, we use the [`prepareData`](https://github.com/sudbasnet/distanceFunction/blob/master/dataPrepration/prepareData.R) function to combine all the above data. It basically intersects the map and events.

```R
events <- prepareData(events, area)
```

If we needed to extract the data for a specific state or district from the `events` data.frame, we can use the `over` function, which basically returns true or NA based on whether a spatial point is inside a spatial polygon.

```R
# if we wanted to get data for state "Andra Pradesh"
events <- events[!is.na(sp::over(events, map_india_districts[,map_india_districts$ST_NM == "Andhra Pradesh"])[,1]),]
```

### **_Adding infrastructure distances_**

Here, we add the infrastructure distances of both types; proximity and density. This means distance to the closest infrastructure of each infrastructure type, and number of infrastructure of a certain type within a 10km radius. The script for this is available [HERE](./scripts/distance_from_infrastructure.R). This script uses following steps:

1. Read the poi files, I saved them in .csv format for ease of use but we can directly use the shapefiles.

```R
poi_india_police <- read.csv("./shapefiles/india_poi/poi_india_police.csv", header = T)
```

2. Then I separate out unique lat, lon values from the `events` data so we dont have to redo calculations.

```R
latlon_event_india_2014 <- sqldf::sqldf("select distinct lat, lon from events;")
```

3. Now I use the `getInfrastructureDistance` function defined in the file mentioned above to calculate the distances between these lat lon of the events file with the positions of infrastructure.

```R
m_police_distance <- getInfrastructureDistance(x = latlon_event_india_2014, poi = poi_india_police, neighborhood_m = 10000.00)
# neighborhood_m means neighborhood in meters.
```

4. Now we assign the variables and normalize them using feature scaling. After which the data is merged into the main events dataframe.

### **_Output the data_**

We can use this data for the Multi-agent simulations, or the spatio-temporal clustering.

```R
write.csv(events, "events_india_2014_repast.csv")
```

For details regarding agent-based social unrest simulation, please see [SocialUnrestMAS](https://github.com/sudbasnet/SocialUnrestMAS).

And, for details regarding clustering, please see [spatiotemporal-KDTree](https://github.com/sudbasnet/spatiotemporal-kdTree).

If you want to do simple clustering in R, please see [distance calculation](./distanceCalculation.md).
