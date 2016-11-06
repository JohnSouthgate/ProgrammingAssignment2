## What the functions do:
# 
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve retrieves the inverse from the cache.
# Computing the inverse of a square matrix is done with the solve function. 
# 
# It is assumed that the matrix supplied is always invertible.
# 

## makeCacheMatrix: Create an object to manage caching of a matrix and its inversion
makeCacheMatrix <-function(x = matrix()) {
	
	im <- NULL
	set <- function(y) {
		x <<- y
		im <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) im <<- inverse
	getinverse <- function() im
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}

## cacheSolve: Return the inverse of a matrix from the cache if it exists there
cacheSolve <- function(x, ...) {

	im <- x$getinverse()
	if(!is.null(im)) {
		message("getting cached data")
		return(im)
	}
	data <- x$get()
	im <- solve(data, ...)
	x$setinverse(im)
	im
}

