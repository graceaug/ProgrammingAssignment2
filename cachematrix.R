## The following two functions attempts to create a matrix to store its inverse and 
## returns the inverse if it can be found in the cache. 
## A new inverse would be calculated and store in cache if the matrix changes.

## The makeCache function creates a matrix object that caches its inverse.

makeCacheMatrix <- function(x = matrix()) {

	## initialise inversematrix to NULL
	inversematrix = NULL

	## set x to a new matrix, and reinitialise inverse matrix
	set <- function(newmatrix) {
		x <<- newmatrix
		inversematrix <<- NULL
	}
	
	## get current matrix
	get <- function() {
		x
	}

	## set new inverse of matrix
	setInverse <- function(inverse_m) {
		inversematrix <<- inverse_m
	}
	
	## get current inverse of matrix
	getInverse <- function() {
		inversematrix
	}
	
	## create a list of functions
	list(set = set, get= get, setInverse=setInverse, getInverse=getInverse)
}


## The cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse was already calculated and matrix has not changed, then cacheSolve would
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {

	## get cached inverse of matrix x
	inversematrix <- x$getInverse()

	## if there is cached inverse matrix, return the result
	if (!is.null(inversematrix)) {
		message("getting cached data")
		return (inversematrix)
	}
	
	## no cached inverse matrix, get my matrix x
	matrix <- x$get()

	## using matrix x, calculate inverse 
	inversematrix <- solve(matrix)
	
	## cache the new inverse matrix
	x$setInverse(inversematrix)

	##return the inverse matrix
	inversematrix
}

