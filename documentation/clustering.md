# Clustering

## Experiments with spatiotemporal clustering
Now, we can apply the `dbscan` function on these distance matrices. To apply the `dbscan` function we need to supply the distance matrix as a `dist` object, the `eps` parameter which is essentially the radius of neighborhood for each event. The last parameter that the `dbscan` function uses is `minPts` , this is the minimum number of points that should be inside a neighborhood of size `eps` for it to be considered a cluster. For our experiments, we will supply these points arbitarily by choosing 4 as the `minPts` and 10% of the maximum distance value in each of the distance matrices.

Our goal is to find closely related events by evaluating the spatiotemporal distance along with the difference in the socioeconomic environment of the event location. We are using DBSCAN as the clustering algorithm as it is efficient in finding clusters of similar densities in spatial database with noise. As this algorithm clusters objects based on the similar densities, it does not perform very well on datasets with varying densities and is very sensitive to the input parameters eps and minPts, the optimal values for these parameters is hard to determine. Hence, we will be choosing values of eps and minPts randomly for this preliminary analysis. For this experiment, we are using 10% of the max distance between all pair of events as the eps and take 4 as the minPts. Note that the distances calculated by the distance function are always in the range of [0-1].
 
### Spatiotemporal distances
While considering only the spatiotemporal distances, we take 10% of the max distance between events as eps and take minPts as 4. Running DBSCAN clustering on the spatiotemporal distances among 1770 events with these parameters, we get 69 clusters and 540 noise points. For our analysis, we will examine three of the largest clusters and then three of the smallest clusters formed after the DBSCAN clustering. In DBSCAN clustering, a cluster might have border points that are shared among different core points of different clusters, in this case the border point is assigned to a cluster at random and so a cluster may have less number of points than the minPts. For the 3 smallest clusters, will only be examining clusters with at least 4 (minPts) points.
![Number of events in each cluster (noise not included), SPATIOTEMPORAL](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Picture1.png)
```dbscan_distanceMatrix_india_2014_northeast_spatiotemporal_max10 <- dbscan(as.dist(distanceMatrix_india_2014_northeast_spatiotemporal), eps=0.1, minPts = 4)```

A summary of the noise points in the data after clustering is shown below.

Cluster | Events | Min. | Median | Mean | Max. | Standard deviation | Min. (Event Date) | Max. (Event Date) | Min. spatial distance (km) | Avg. spatial distance (km) | Max. spatial distance (km)
----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | -----------
0 (noise) | 540 | 0 | 1 | 0.9275 | 1 | 0.1578 | 1/1/14 | 12/31/14 | 0 | 250.71 | 727.23

Similarly, a summary of the largest three clusters and three smallest clusters are listed in table 3 and table 4 respectively.

Cluster | Events | Min. | Median | Mean | Max. | Standard deviation | Min. (Event Date) | Max. (Event Date) | Min. spatial distance (km) | Avg. spatial distance (km) | Max. spatial distance (km)
----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | -----------
14 | 154 | 0 | 0.7477 | 0.6873 | 1 | 0.2662 | 3/20/14 | 5/12/14 | 0 | 263.01 | 723.34
51 | 116 | 0 | 0.6975 | 0.6458 | 1 | 0.2638 | 10/5/14 | 11/11/14 | 0 | 243.27 | 723.33
9 | 106 | 0 | 0.6991 | 0.651 | 1 | 0.2765 | 2/14/14 | 3/28/14 | 0 | 243.69 | 723.33

Cluster | Events | Min. | Median | Mean | Max. | Standard deviation | Min. (Event Date) | Max. (Event Date) | Min. spatial distance (km) | Avg. spatial distance (km) | Max. spatial distance (km)
----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | -----------
61 | 4 | 0.05 | 0.1017 | 0.2371 | 0.9249 | 0.3399 | 11/19/14 | 11/26/14 | 0 | 62.53 | 125.07
63 | 4 | 0.0167 | 0.0575 | 0.247 | 0.6688 | 0.3262 | 12/12/14 | 12/14/14 | 0 | 37.16 | 74.31
69 | 4 | 0 | 0.1 | 0.1 | 0.1666 | 0.0632 | 5/28/14 | 6/7/14 | 0 | 0 | 0

Plots for the largest and the smallest clusters formed with clustering based on spatiotemporal distances is shown below, note that the administrative divisions in the map for spatiotemporal clustering is shown on district level.
![Cluster - 0 (Noise)](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_socioeconomic_noise_max10.png)
![Cluster - 14](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_spatiotemporal_cluster14_max10.png)
![Cluster - 51](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_spatiotemporal_cluster51_max10.png)
![Cluster - 9](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_spatiotemporal_cluster9_max10.png)
![Cluster - 61](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_spatiotemporal_cluster61_max10.png)
![Cluster - 63](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_spatiotemporal_cluster63_max10.png)
![Cluster - 69](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_spatiotemporal_cluster69_max10.png)
