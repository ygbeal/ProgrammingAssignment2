## The following functions -- makeCacheMatrix and cacheSolve, will
## cache the inverse of a matrix 
## Matrix inversion is usually a costly computation and their may be
## some benefit to cashing the inverse of a matrix than compute it
## repeatedly

## This function (makeCacheMatrix) create a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
        
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

## This function (cacheSolve) computes the inverse of the special
## "matrix" returned by makeCacheMatrix function. If the inverse
## already been calculated (and the matrix has not changed), then
## this function should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
