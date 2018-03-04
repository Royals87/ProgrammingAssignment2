## Matrix inversion can be costly, so there may be a benefit to caching the inverse rather
## than computing it repeatedly.

## makeCacheMatrix will create a "matrix" object that can cache its inverse by:
##	1. setting the value of the matrix
##	2. getting the value of the matrix
##	3. setting the value of the inverse of that matrix
##	4. getting the value of the inverse of that matrix
## x is always an invertible square matrix

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function(y) {
			x <<- y
			inv <<- NULL
		}
		get <- function() x
		setinv <- function(inverse) inv <<- solve(x)  ## calculate the inverse
		getinv <- function() inv
		list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve calculates the inverse of the matrix calculated in makeCacheMatrix by:
##	1. checking first to see if the inverse has already been calculated
##		a. if so, it will retrieve the inverse from the cache and skip calculating
##	2. calculating the inverse of the matrix and setting the value in the cache

cacheSolve <- function(x, ...) {
		inv <- x$getinv()
		if(!is.null(inv)) {
			message("getting cached data")
			return(inv)
		}
		data <- x$get()
		inv <- solve(data)
		x$setinv(inv)
		inv
}
