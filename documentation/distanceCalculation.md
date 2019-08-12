# Distance Calculation

The distance function is currently only defined to calculate distances between two events with geographic coordinates, time stamps and some socioeconomic variables in numberic form. So once the data is prepared, we can actually go ahead and calculate distances between various events.
We will define a function that intakes events as vectors and then calculates the distance between them. I have created a function with the name `distanceFunction`.
The link to the function's defination is [here](./scripts/distanceFunction.R).

Next, we will form another function that will use the above function to formulate a distance matrix with the distances between each pair of events. A distance matrix is preferred because the `dbscan` of the **dbscan** package in R, can use an object of class `dist` which is basically a distance matrix. For more details about the package **dbscan**, please see the following [page](https://cran.r-project.org/web/packages/dbscan/dbscan.pdf).
The function is named `distanceFunction_processData` and returns a distance matrix. Note that the returned matrix is not a `dist` class object.

To run the distance function we do the following:

```R
# possible values for spatialORsocioORinfra are: "spatiotemporal", "socioeconomic", "infrastructure", "spatiotemporal_infrastructure", "spatiotemporal_socioeconomic", or "all"

distanceMatrix_india_2014_northeast_spatiotemporal<- distanceFunction_processData(events_northeast, 100, 30, var_count = 0, spatialORsocioORinfra = 'spatiotemporal')

distanceMatrix_india_2014_northeast_socioeconomic<- distanceFunction_processData(events_northeast, 100, 30, var_count = 2, spatialORsocioORinfra='socioeconomic')

distanceMatrix_india_2014_northeast_combined<- distanceFunction_processData(events_northeast, 100, 30, var_count = 2, infra_count = 6, spatialORsocioORinfra='spatiotemporal_socioeconomic')
```

Here we are applying the function with the threshold parameters as 100 km and 30 days to a subset of data that only includes the northeastern states of India.

The above run will give us distance matrices, to convert them into `dist` objects, we must use the function `as.dist()` on them.

After the above matrics are formed, lets look at the summary of the formed distance matrics.

```
summary(as.dist(distanceMatrix_india_2014_northeast_spatiotemporal))
```

| Min.   | 1st Qu. | Median | Mean   | 3rd Qu. | Max.   |
| ------ | ------- | ------ | ------ | ------- | ------ |
| 0.0000 | 0.9806  | 1.0000 | 0.9287 | 1.0000  | 1.0000 |

lets do the same for socioeconomic distances and the combined one.

```
summary(as.dist(distanceMatrix_india_2014_northeast_socioeconomic))
```

| Min.   | 1st Qu. | Median | Mean   | 3rd Qu. | Max.   |
| ------ | ------- | ------ | ------ | ------- | ------ |
| 0.0000 | 0.1126  | 0.1855 | 0.2090 | 0.2836  | 0.9005 |

```
summary(as.dist(distanceMatrix_india_2014_northeast_combined))
```

| Min.   | 1st Qu. | Median | Mean   | 3rd Qu. | Max.   |
| ------ | ------- | ------ | ------ | ------- | ------ |
| 0.0000 | 0.5351  | 0.5824 | 0.5688 | 0.6333  | 0.9503 |

After we look at the result, we can say that adding socioeconomic distances to the spatiotemporal restricts some events to be connected to each other because they are not in similar socioeconomic environment.

To see some preliminary clustering, please see [Clustering](./clustering.md). Note that this clustering does not use the spatiotemoral-KD-Trees.
