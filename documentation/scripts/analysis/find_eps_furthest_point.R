# input is a sorted array from the function kNNdist.
# note that the array returned by kNNdist is not sorted, we must apply sort() to it.
find_eps <- function(K){
	maxIndex <- 0;
	maxDist <- -1;
	N <- length(K);
	x1 <- 1;
	y1 <- K[x1];
	x2 <- N;
	y2 <- K[x2];
	y2_y1 <- y2 - y1
	x2_x1 <- x2 - x1
	x2y1_y2x1 <- (x2 * y1) - (y2 * x1)
	
	denominator <- sqrt((y2_y1 * y2_y1)+ (x2_x1 * x2_x1))
	for (i in 1:N){
		x0 <- i;
		y0 <- K[x0];
		currDist <- abs( (y2_y1 * x0) - (x2_x1 * y0) + x2y1_y2x1 ) / denominator;
		if (maxIndex == 0 || currDist > maxDist){
			maxDist <- currDist;
			maxIndex <- i; 
		}
	}
	return(K[maxIndex])
}