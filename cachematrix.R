## Matrix inversion is potentially time-consuming, particullarly when repeated 
## inversion must be performed on the same matrix. This function is to creates 
## a special "matrix" object that can cache its inverse  If the inverse has 
## already been calculated (and the matrix has not changed), then this function 
##retrieve the inverse from the cache without repeating the operation. 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Creates a special "matrix" object that can cache its inverse.
    #
    # Args:
    #   x: One matrix used to initialize the cacheMatrix object.
    #
    # Returns:
    #   An object that can cache the inverse of the input matrix.
    invx <- NULL
    set <- function(y) {
        x <<- y
        invx <<- NULL
    }
    get <- function() x
    setMatrixInverse <- function(invMatrix) invx <<- invMatrix
    getMatrixInverse <- function() invx
    list(set = set, get = get,
         setMatrixInverse = setMatrixInverse,
         getMatrixInverse = getMatrixInverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    # Creates a special "matrix" object that can cache its inverse.
    #
    # Args:
    #   x: matrix object that can cache its inverse. 
    #
    # Returns:
    #   the inverse of the matrix.
    
    ## Return a matrix that is the inverse of 'x'
    invx <- x$getMatrixInverse()
    if(!is.null(invx)) {
        message("getting cached inversed matrix")
        return(invx)
    }
    data <- x$get()
    invx <- solve(data, ...)
    x$setMatrixInverse(invx)
    invx
}
