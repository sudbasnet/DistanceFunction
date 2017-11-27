# Distance Calculation

The distance function is currently only defined to calculate distances between two events with geographic coordinates, time stamps and some socioeconomic varibales in numberic form. So once the data is prepared, we can actually go ahead and calculate distances between various events. 
We will define a function that intakes events as vectors and then calculates the distance between them. For lack of a better name, I have created a function with the name `distanceFunction_vectors`.
The link to the function's defination is [here](https://github.com/sudbasnet/distanceFunction/blob/master/function/distanceFunction_vectors.r).

Next, we will form another function that will use the above function to formulate a distance matrix with the distances between each pair of events. A distance matrix is preferred because the `dbscan` of the **dbscan** package in R, can use an object of class `dist` which is basically a distance matrix. For more details about the package **dbscan**, please see the following [page](https://cran.r-project.org/web/packages/dbscan/dbscan.pdf).
The function (for lack of better names :D ) is named `distanceFunction_using_vectors` and returns a distance matrix. Note that the returned matrix is not a `dist` class object.
The function body is given [here](https://github.com/sudbasnet/distanceFunction/blob/master/function/distanceFunction_using_functionVectors.R).

To run the distance function we do the following:
```
distanceMatrix_india_2014_northeast_spatiotemporal<- distanceFunction_using_vectors(data2_india_2014_northeast, 100, 30, var_count = 0, spatialORtemporal='spatiotemporal')

distanceMatrix_india_2014_northeast_socioeconomic<- distanceFunction_using_vectors(data2_india_2014_northeast, 100, 30, var_count = 2, spatialORtemporal='socioeconomic')

distanceMatrix_india_2014_northeast_combined<- distanceFunction_using_vectors(data2_india_2014_northeast, 100, 30, var_count = 2, spatialORtemporal='both')
```
Here we are applying the function with the threshold parameters as 100 km and 30 days.

The above run will give us distance matrices, to convert them into `dist` objects, we must use the function `as.dist()` on them.

After the above matrics are formed, lets look at the summary of the formed distance matrics.
```
summary(as.dist(distanceMatrix_india_2014_northeast_spatiotemporal))
```
Min. | 1st Qu. | Median | Mean | 3rd Qu. | Max. 
-------- | -------- | -------- | -------- | -------- | --------
 0.0000 | 0.9806 | 1.0000 | 0.9287 | 1.0000 | 1.0000 

lets do the same for socioeconomic distances and the combined one.
```
summary(as.dist(distanceMatrix_india_2014_northeast_socioeconomic))
```
Min. | 1st Qu. | Median | Mean | 3rd Qu. | Max. 
-------- | -------- | -------- | -------- | -------- | --------
 0.0000 | 0.1126 | 0.1855 | 0.2090 | 0.2836 | 0.9005 

```
summary(as.dist(distanceMatrix_india_2014_northeast_combined))
```
Min. | 1st Qu. | Median | Mean | 3rd Qu. | Max. 
-------- | -------- | -------- | -------- | -------- | --------
0.0000 | 0.5351 | 0.5824 | 0.5688 | 0.6333 | 0.9503 

After we look at the result, we can say that adding socioeconomic distances to the spatiotemporal restricts some events to be connected to each other because they are not in similar socioeconomic environment.

Now, we can apply the `dbscan` function on these distance matrices. To apply the `dbscan` function we need to supply the distance matrix as a `dist` object, the `eps` parameter which is essentially the radius of neighborhood for each event. The last parameter that the `dbscan` function uses is `minPts` , this is the minimum number of points that should be inside a neighborhood of size `eps` for it to be considered a cluster. For our experiments, we will supply these points arbitarily by choosing 4 as the `minPts` and 10% of the maximum distance value in each of the distance matrices.
Running DBSCAN clustering on the spatiotemporal distances among 1770 events with `minPts` 4 and `eps` of 0.1, we get 69 clusters and 540 noise points. For our analysis, we will examine three of the largest clusters and then three of the smallest clusters formed after the DBSCAN clustering. In DBSCAN clustering, a cluster might have border points that are shared among different core points of different clusters, in this case the border point is assigned to a cluster at random and so a cluster may have less number of points than the minPts. For the 3 smallest clusters, will only be examining clusters with at least 4 (minPts) points. The results are summaized in the plot below.
![Number of events in each cluster (noise not included) SPATIOTEMPORAL](https://octodex.github.com/images/yaktocat.png)
