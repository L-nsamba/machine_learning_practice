add2 <- function(x, y){
	x + y
}

above10 <- function(x) {
	use <- x > 10
	x[use]
}

above <- function(x, n) {
	use <- x > n
	x[use]
}

columnmean <- function(y) {
	nc <- ncol(y)
	mean <- numeric(nc)
	for (i in 1:nc) {
		mean[i] <- mean(y[,i])
	}
	mean
}