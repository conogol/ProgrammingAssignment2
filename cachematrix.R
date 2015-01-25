# Matrix inversion is usually a costly computation and there may be some benefit to caching 
# the inverse of a matrix rather than computing it each and every time
# These functions allow caching the inverse of a matrix


# makeCacheMatrix() Creates a list with the following functions:
#    $set() sets the value of a matrix, 
#    $get() gets the value of a matrix
#    $setinverse() sets the value of the inverse of the matrix
#		 $getinverse() gets the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	inve <- NULL
	set <- function(y) {
		x <<- y
		inve <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inve <<- inverse
	getinverse <- function() inve
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve() Returns the inverse of a matrix.
# If the inverse of the matrix has already been computed, it will retrieve it from cache.
# If, however, there is a cache miss, the function will compute the inverse and cache it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
	inve <- x$getinverse()
	if(!is.null(inve)) {
		#message("Fetching cached data")
		return(inve)
	}
	data <- x$get()
	inve <- solve(data)
	x$setinverse(inve)
	inve
}
