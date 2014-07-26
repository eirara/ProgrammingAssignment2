## This R file contains functions for creating a matrix and computing
## its inverse. It features caching the inverse of a given matrix in
## its initial computation.

## makeCacheMatrix() function is for creating a special object that
## stores a matrix and cache's its inverse. It has sub-functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(
        set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse
    )
}


## cacheSolve() function calculates the inverse of the special matrix made
## by makeCacheMatrix() above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve() simply retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("Getting cached data ...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
