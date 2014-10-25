## The makeCacheMatrix function will create a cache for a matrix. The cache is used to
## store the inverse of the matrix. The variable i is used to store the cached value
## If i is NULL, it means that the inverse has not been calculated since
## the last time the value of the matrix was changed. 

## The cache is a list with the functions:

## get        : returns the value of the matrix
## set        : sets the matrix to the given value. The variable i is
##              assigned NULL to invalidate the cache. 
## setinverse : stores the given value (which must be the matrix's inverse)
##              in the cache variable, i
## getinverse : returns the value of i.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function () i
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of a matrix created by makeCacheMatrix.
## The cache is first checked to see if the inverse has been calculated previously.
## If so the cached value is returned. Otherwise, the inverse is calculated and stored in
## the cache before being returned to the caller.
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("Getting cached data")
                i
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
