## This pair of functions, makeCacheMatrix() and cacheSolve(), are used to cache
## the inverse of a matrix.  Caching the inverse of a matrix is helpful for
## improving performance (especially for large matrices) because inverting a
## matrix is usually a costly computation.


## This function, makeCacheMatrix(), creates a special "matrix" object
## that can cache its inverse.
##
## It returns a list containing functions to:
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse of the matrix
##   4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  	# variables: x is the matrix, inv is the inverse
        inv <- NULL

	# set value of matrix, and reset inverse to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        # get value of matrix
        get <- function() x

        # set inverse
        setInverse <- function(Inverse) inv <<- Inverse

        # get inverse
        getInverse <- function() inv

        # create and return the list of functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by
## the makeCacheMatrix() function above.  If the inverse has already been
## calculated, then cacheSolve() just returns the inverse from the cache.

cacheSolve <- function(x, ...) {
  	# variables: x is the special matrix object, inv is the inverse

        # get cached inverse
  	inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        data <- x$get()
        # solve() finds the inverse of a matrix
        inv <- solve(data, ...)

        # cache the inverse and return the inverse
        x$setInverse(inv)
        inv
}
