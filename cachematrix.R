## Matrix inversion is usually a costly computation and there may be
## some benefit to caching the inverse of a matrix rather than
## computing it repeatedly. Here I write a pair of functions that
## cache the inverse of a matrix.

## This function stores the inverted matrix, and sets up various
## methods for accessing it (matrices are assumed to be invertible)

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
        set <- function(y) {
                x <<- y
		m <<- NULL
        }
        get <- function() x
        setinverse <- function(invert) m <<- invert
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function checks for a cached version of the inverted matrix;
## if it does not find it, it performs the inversion

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x[['getinverse']]()
	## if found (not NULL), return the inverse and finish
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
	## if not found, solve the matrix and set the result as the inverse
        data <- x[['get']]()
        m <- solve(data, ...)
        x[['setinverse']](m)
        m
}
