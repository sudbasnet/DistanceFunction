# 1. DATA PREPRATION

First I load the libraries that we'll need for this analysis with the folowing sapply.
```
sapply (c("sp", "raster", "spatstat", "maptools", "plotrix", "scatterplot3d", "assertthat", "geosphere","dbscan", "rgdal", "rgeos", "spatstat"), library, character.only = TRUE)
```

I have divided the states of India into 4 parts; northeast, north, south and west on state level and district level both. So the next thing I will do is load these shapefiles into R. (These shape-files have been made available in the [shapefiles](https://github.com/sudbasnet/distanceFunction/tree/master/shapefiles) folder of the repository)
``` 
# states
map_india_west <- shapefile("./shapefiles/map_india_west.shp")
map_india_northeast <- shapefile("./shapefiles/map_india_northeast.shp")
map_india_south <- shapefile("./shapefiles/map_india_south.shp")
map_india_north <- shapefile("./shapefiles/map_india_north.shp")
# districts
map_india_west_d <- shapefile("./shapefiles/map_india_west_d.shp")
map_india_northeast_d <- shapefile("./shapefiles/map_india_northeast_d.shp")
map_india_south_d <- shapefile("./shapefiles/map_india_south_d.shp")
map_india_north_d <- shapefile("./shapefiles/map_india_north_d.shp")
```

Next, i would like to assign the population and some socioeconomic data values from the census-2011 dataset of India made public by the Indian government. I'd like to assign these values to the district level data. 
```
census_abstract_india_2011 <- read.csv("./pca-full.csv", sep=",", header=T)
# only keep the data that is for TOTAL (=Urban+Rural) and only pick the columns I think we need
census_abstract_india_2011 <- census_abstract_india_2011[ (toupper(census_abstract_india_2011$TRU)=='TOTAL') & (toupper(census_abstract_india_2011$Level)=='DISTRICT'),c(1,2,4,6,8,11,20,26,29)]
names(census_abstract_india_2011) <- c("State", "District", "Name", "area_km_sq","population_total", "population_age0to6", "population_literate", "population_total_worker", "population_main_workforce")
```

Now I used the `merge` function of R to pull the area and population values from the census dataset into the shapefiles. This is a really powerful function and perfect if u want to merge two datasets. As we can see, this function can actually combine a data of class SpatialObject and regular data.frame. 
```
map_india_northeast_d <- merge(map_india_northeast_d, census_abstract_india_2011[,c("District","area_km_sq","population_total")], by.x="censuscode", by.y="District", all = FALSE)
map_india_west_d <- merge(map_india_west_d, census_abstract_india_2011[,c("District", "area_km_sq", "population_total")], by.x="censuscode", by.y="District", all = FALSE)
map_india_north_d <- merge(map_india_north_d, census_abstract_india_2011[,c("District", "area_km_sq", "population_total")], by.x="censuscode", by.y="District", all = FALSE)
map_india_south_d <- merge(map_india_south_d, census_abstract_india_2011[,c("District", "area_km_sq", "population_total")], by.x="censuscode", by.y="District", all = FALSE)
# all = FALSE means we only keep the ones that merge 
```

The distance function assumes that the variable data would be available separately from the area shapefiles, so I created a variable dataset separately event though it will be a part of the area and I could have done this in the previous step.
```
variable_india_northeast_d <- merge(map_india_northeast_d, census_abstract_india_2011[,c("District", "population_age0to6", "population_literate", "population_total_worker", "population_main_workforce")], by.x="censuscode", by.y="District", all = FALSE)
```
Note that I only created a variable-set for northeast india. This is because I realized the actual events dataset is the smallest for northeast, and I will only be able to work on northeast in the time I have decided to give this analysis.


Next, let me import the events data pulled from GDELT's event database for the year 2014 for India. We have choosen this year as it was the election year of India's current PM, Narendra Modi. We expect to have a moderate number of events throughout the country in this year. Since we are usign the census data from 2011, it is important to note that we are assuming these census varaible to be consistent enough that we can use it for 2014.
```
events_india_GDELT_2014 <- read.csv("events_india_GDELT_2014.csv",header=T, sep="," )
events_india_GDELT_2014$event_date <- as.Date(substr(events_india_GDELT_2014$event_date, 1, 10),format = "%Y-%m-%d")
```
Now that we have the events data imported in the format we have for the SURGE database, we need to transform it into a data.frame structure that the distance function can automatically work with. I have used the function [`prepareEvent`](https://github.com/sudbasnet/distanceFunction/blob/master/dataPrepration/prepareEvent.R) that is available in the repository "link".
```
events_india_2014 <- prepareEvent(events_india_GDELT_2014, lat = "latitude", lon = "longitude", tstart = "event_date", tend = "event_date", eventCategory = "event_category", url = "url", uniqueid = "event_id")
```

Similarly, I'll use the [`prepareArea`](https://github.com/sudbasnet/distanceFunction/blob/master/dataPrepration/prepareArea.R) function to put the area shapefiles into a fixed structure.
```
area_india_northeast_d <- prepareArea(map_india_northeast_d, "censuscode", "ST_NM", "DISTRICT", "population_total", "area_km_sq", "district")
```

Now, from the events data.frame, I will just extract the data for northeast India. I used the `over` function, which basically returns true or NA based on whether a spatial point is inside a spatial polygon (northeast shapefiles).
```
events_india_2014_northeast <- events_india_2014[!is.na(sp::over(events_india_2014, map_india_northeast_d)[,1]),]
```

And finally the [`prepareData`](https://github.com/sudbasnet/distanceFunction/blob/master/dataPrepration/prepareData.R) function to combine all the above data.
```
data2_india_2014_northeast <- prepareData(events_india_2014_northeast, area_india_northeast_d, variable_india_northeast_d)
```

[Next: Calculating Distances](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/distanceCalculation.md)
