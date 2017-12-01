# 3. Clustering

## 3.1 Experiments with spatiotemporal clustering
Now, we can apply the `dbscan` function on these distance matrices. To apply the `dbscan` function we need to supply the distance matrix as a `dist` object, the `eps` parameter which is essentially the radius of neighborhood for each event. The last parameter that the `dbscan` function uses is `minPts` , this is the minimum number of points that should be inside a neighborhood of size `eps` for it to be considered a cluster. For our experiments, we will supply these points arbitarily by choosing 4 as the `minPts` and 10% of the maximum distance value in each of the distance matrices.

Our goal is to find closely related events by evaluating the spatiotemporal distance along with the difference in the socioeconomic environment of the event location. We are using DBSCAN as the clustering algorithm as it is efficient in finding clusters of similar densities in spatial database with noise. As this algorithm clusters objects based on the similar densities, it does not perform very well on datasets with varying densities and is very sensitive to the input parameters eps and minPts, the optimal values for these parameters is hard to determine. Hence, we will be choosing values of eps and minPts randomly for this preliminary analysis. For this experiment, we are using 10% of the max distance between all pair of events as the eps and take 4 as the minPts. Note that the distances calculated by the distance function are always in the range of [0-1].
 
### 3.1.1 Spatiotemporal distances
While considering only the spatiotemporal distances, we take 10% of the max distance between events as eps and take minPts as 4. Running DBSCAN clustering on the spatiotemporal distances among 1770 events with these parameters, we get 69 clusters and 540 noise points. For our analysis, we will examine three of the largest clusters and then three of the smallest clusters formed after the DBSCAN clustering. In DBSCAN clustering, a cluster might have border points that are shared among different core points of different clusters, in this case the border point is assigned to a cluster at random and so a cluster may have less number of points than the minPts. For the 3 smallest clusters, will only be examining clusters with at least 4 (minPts) points.

Min. | 1st Qu. | Median | Mean | 3rd Qu. | Max.
----------- | ----------- | ----------- | ----------- | ----------- | -----------
0 | 0.9806 | 1 | 0.9287 | 1 | 1

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
![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_spatiotemporal_noise_max10.png "Cluster 0 (Noise)")
![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_spatiotemporal_cluster14_max10.png  "Cluster 14")
![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_spatiotemporal_cluster51_max10.png "Cluster 51")
![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_spatiotemporal_cluster9_max10.png "Cluster 9")
![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_spatiotemporal_cluster61_max10.png "Cluster 61")
![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_spatiotemporal_cluster63_max10.png "Cluster 63")
![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_spatiotemporal_cluster69_max10.png "Cluster 69")

### 3.1.2 Socioeconomic distances
Similarly, we now look at the DBSCAN clustering based on only socioeconomic distances. Using the same technique for eps selection for socioeconomic distances we only get 5 clusters with almost all events in the 1st cluster. A summary of the socioeconomic distances between events is shown in Table 5.

Min. | 1st Qu. | Median | Mean | 3rd Qu. | Max.
----------- | ----------- | ----------- | ----------- | ----------- | -----------
0 | 0.1126 | 0.1855 | 0.209 | 0.2836 | 0.9005


![Number of events in each cluster (noise not included), SPATIOTEMPORAL](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/socioeconomic_clustering_structure.png)

A summary of the clusters formed by clustering of all events based on socioeconomic distances are shown below. Note that the values of Literacy Rate and Worker population used are the normalized values that are used in the distance calculation.

Cluster | Events | Min. | Median | Mean | Max. | Standard deviation | Avg. Literacy Rate | Standard Deviation Literacy Rate | Avg. Worker Pop. | Standard Deviation Worker Pop.
----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | -----------
0 (noise) | 1 | 0 | 0 | 0 | 0 | 0 | 0.4764 | 0 | 0.8235 | 0
1 | 1721 | 0 | 0.181 | 0.1992 | 0.7688 | 0.1239 | 0.5687 | 0.1839 | 0.2774 | 0.1791
2 | 7 | 0 | 0 | 0 | 0 | 0 | 0.736 | 0 | 0.8401 | 0
3 | 31 | 0 | 0 | 0 | 0 | 0 | 0.9995 | 0 | 0.5546 | 0
4 | 5 | 0 | 0.0789 | 0.0473 | 0.0789 | 0.0407 | 0.9756 | 0.0223 | 0.8671 | 0.0641
5 | 5 | 0 | 0 | 0 | 0 | 0 | 0 | 0 | 0.1363 | 0

Plots for the results listed for socioeconomic clustering are shown below. Note that the administrative division shown in the maps below are on district level.

![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_socioeconomic_noise_max10.png "Cluster 0 (Noise)")
![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_socioeconomic_cluster1_max10.png "Cluster 1")
![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_socioeconomic_cluster2_max10.png "Cluster 2")
![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_socioeconomic_cluster3_max10.png "Cluster 3")
![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_socioeconomic_cluster4_max10.png "Cluster 4")
![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_socioeconomic_cluster5_max10.png "Cluster 5")

### 3.1.3 Combined spatiotemporal and socioeconomic distances
Now we consider a distance function that is a weighted sum of the spatio-temporal and socio-economic distances, equally weighted.   A summary of these distance values is shown in Table 9.  Sure enough, as socio-economic distances are closer for each of events, the mean distance between each pair of events under this composite distance function is now significantly smaller compared to that for spatio-temporal distance alone (0.5688 vs. 0.9287).   As a result, more events are within “clustering” neighborhood of each other’s, yielding a much larger number of clusters, as shown next.

Min. | 1st Qu. | Median | Mean | 3rd Qu. | Max.
----------- | ----------- | ----------- | ----------- | ----------- | -----------
0 | 0.5351 | 0.5824 | 0.5688 | 0.6333 | 0.9503

DBSCAN clustering based on the combined spatiotemporal and socioeconomic distances on the same dataset gives 86 clusters with 629 noise points. The eps value is 10% of the max distance among events, 0.095026, and minPts is still 4.

![Number of events in each cluster (noise not included), SPATIOTEMPORAL](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/spatiotemporal_socioeconomic_cluslterStructure.png)

A summary of the noise points, 3 largest clusters and 3 of the smallest clusters is shown below.

Cluster | Events | Min. | Median | Mean | Max. | Standard deviation | Min. Event date | Max. Event date | Min. Spatial Distances (km) | Avg. Spatial Distances (km) | Max. Spatial Distances (km) | Avg. Literacy rate | Standard deviation Literacy rate | Avg. Worker Population | Standard deviation Worker Population
----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | -----------
0 | 629 | 0.0083 | 0.598 | 0.5897 | 0.9503 | 0.1141 | 1/1/14 | 12/31/14 | 0 | 263.81 | 751.99 | 0.5218 | 0.2026 | 0.3373 | 0.2338

Cluster | Events | Min. | Median | Mean | Max. | Standard deviation | Min. Event date | Max. Event date | Min. Spatial Distances (km) | Avg. Spatial Distances (km) | Max. Spatial Distances (km) | Avg. Literacy rate | Standard deviation Literacy rate | Avg. Worker Population | Standard deviation Worker Population
----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | -----------
10 | 156 | 0 | 0.5043 | 0.4336 | 0.6303 | 0.1491 | 2/5/14 | 8/2/14 | 0 | 195.89 | 574.27 | 0.6741 | 0.1006 | 0.168 | 0.0458
8 | 78 | 0 | 0.4345 | 0.3916 | 0.5943 | 0.1571 | 1/28/14 | 4/8/14 | 0 | 173.42 | 521.26 | 0.5693 | 0.0912 | 0.1729 | 0.065
4 | 60 | 0 | 0.25 | 0.2251 | 0.5566 | 0.0891 | 1/4/14 | 6/25/14 | 0 | 16.25 | 455.52 | 0.8134 | 0.0034   | 0.29 | 0.0259

Cluster | Events | Min. | Median | Mean | Max. | Standard deviation | Min. Event date | Max. Event date | Min. Spatial Distances (km) | Avg. Spatial Distances (km) | Max. Spatial Distances (km) | Avg. Literacy rate | Standard deviation Literacy rate | Avg. Worker Population | Standard deviation Worker Population
----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | -----------
83 | 4 | 0.0083 | 0.0583 | 0.0611 | 0.1167 | 0.0496 | 12/15/14 | 12/29/14 | 0 | 0 | 0 | 0.8158 | 0 | 0.6295 | 0
85 | 4 | 0.0083 | 0.0808 | 0.0684 | 0.093 | 0.0312 | 12/24/14 | 12/27/14 | 0 | 21.8 | 30.75 | 0.3781 | 0 | 0.1914 | 0
86 | 4 | 0.0083 | 0.0542 | 0.0583 | 0.1083 | 0.0418 | 2/21/14 | 3/6/14 | 0 | 0 | 0 | 0.335 | 0 | 0.1847 | 0

Plots for the above clusters are shown below:

![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_combined_noise_max10.png "Cluster 0 (Noise)")
![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_combined_cluster10_max10.png "Cluster 10")
![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_combined_Cluster8_max10.png.png "Cluster 8")
![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_combined_Cluster4_max10.png "Cluster 4")
![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_combined_Cluster83_max10.png "Cluster 83")
![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_combined_Cluster85_max10.png "Cluster 85")
![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/plots/Rplot02_combined_Cluster86_max10.png "Cluster 86")

## 3.2 Clustering Goodness
A good quality clustering should produce clusters with high intra-cluster similarity and low inter-cluster similarity. We can calculate the goodness of our clustering by calculating the mean intra-cluster distance and mean inter-cluster distance. The goodness of the clustering is then given by:
![alt text](https://github.com/sudbasnet/distanceFunction/blob/master/documentation/img/goodness_fomula.png "Cluster 0 (Noise)")

To calculate the mean intra-cluster distance and inter-cluster distances, we must first calculate the centroid of each cluster. The centroid of each cluster is a virtual event whose attributes are the averages of attributes from all events in the cluster.

### 3.2.1 Intra-cluster distance
The intra-cluster distance is the measure of how the points inside each cluster are distributed. We can calculate the inter-cluster distance by taking the average of each member event’s distance with the centroid of its cluster, or directly calculate the average distance between each pair of member events inside a cluster.  The intra-cluster distance for the clustering performed with each type of distance is listed in Table 13.

Distance type | mean_intra_cluster_distance centroid to points | mean_intra_cluster_distance point to point
------------- | --------------- | -------------
Spatiotemporal | 0.4781 | 0.5450
Socio-economic | 0.1434 | 0.1886
Combined | 0.2043 | 0.2411

### 3.2.2 Inter-cluster distance
There are multiple ways of calculating the inter-cluster distance of clustering results as well. We look at the following four methods of calculating the mean inter-cluster distance of our clustering technique.

#### 3.2.2.1 Distance of centroid to points in other clusters
In this method, we calculate the distance of each cluster’s centroid with all the points in other clusters. The mean of all the calculated distances is the mean inter-cluster distance of the clustering technique. The columns ‘centroid to points’ and ‘point to point’ reflects the calculation made in Table 13.

Distance type | " mean_intra_cluster_distance (Table13)
centroid to points" | " mean_intra_cluster_distance (Table13)
point to point" | mean_inter_cluster_distance | "Goodness
centroid to points" | "Goodness
point to point"
------------------ | ------------------ | ------------------ | ------------------ | ------------------ | ------------------
Spatiotemporal | 0.4781 | 0.545 | 0.9321 | 1.9496 | 1.7103
Socio-economic | 0.1434 | 0.1886 | 0.4016 | 2.8006 | 2.1294
Combined | 0.2043 | 0.2411 | 0.5694 | 2.7871 | 2.3617



