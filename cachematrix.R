## Matrix inversion cache function to cache the inverse of a matrix rather than compute it repeatedly.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## set the value of the matrix
## get the value of the matrix
## set the value of the iverse
## get the value of the iverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## it first checks to see if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets
## the value of the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
