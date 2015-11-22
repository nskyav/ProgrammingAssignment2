## Creates an object to store a square matrix and it's cached inverse
## Returns a list of set/get pairs:
##   set/get       - for the matrix
##   setinv/getinv - for the inverse of this matrix
makeCacheMatrix <- function(m = matrix()) {
	## default value for cached data
	inv_m <- NULL
	## set/get pair for data
	set <- function(new_m) {
		m <<- new_m
		inv_m <<- NULL
	}
	get <- function() m
	## set/get pair for cached data
	setinv <- function(new_inv) inv_m <<- new_inv
	getinv <- function() inv_m
	## returns a list
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Returns an inverse of the matrix stored in x
## Uses cached data whether it's possible
## x should be created with makeCacheMatrix
## Suggested scenario may look like:
##   m1 <- makeCacheMatrix(matrix(1:4, 2, 2))
##   im <- cacheSolve(m1)
cacheSolve <- function(x, ...) {
	## get the cached inverse
	inv_m <- x$getinv()
	if (!is.null(inv_m)) {
		## and return it if it's not NULL
		return(inv_m)
	}
	## calculate and save the inverse
	inv_m <- solve(x$get(), ...)
	x$setinv(inv_m)
	## and return it
	inv_m
}
