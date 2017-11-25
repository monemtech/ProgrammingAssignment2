## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly. The two functions below are used 
## to cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	mat_inv <- NULL
	
	set <- function(y) {
		x <<- y
		mat_inv <<- NULL
	}
	
    get <- function() x
    setinverse <- function(inverse) mat_inv <<- inverse
    getinverse <- function() mat_inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve 
## should retrieve the inverse from the cache. This function assumes the matrix is always invertible.

cacheSolve <- function(x, ...) {
	mat_inv <- x$getinverse()
		if(!is.null(mat_inv)) {
    message("getting cached data.")
    return(mat_inv)
  }
	data <- x$get()
	mat_inv <- solve(data)
	x$setinverse(mat_inv)
	mat_inv	
}
