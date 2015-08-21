################################################################################
## This R file contains two functions for calculating and caching the inverse of
## a matrix. One function calculates and stores matrix data into a cache by
## using the <<- operator to preserve state. The other function checks whether
## the cache data is present. If so, it uses cached data. If not, it calculates
## the matrix inverse and stores uses the cache-making function to store the
## result.

## makeCacheMatrix creates a special matrix, which is actually a list containing
## a function to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse matrix
## 4) get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
            x <<- y
            i <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) i <<- solve
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse=getInverse)
}

## cacheSolve calculates the inverse of the special matrix created by 
## makeCacheMatrix. It checks to see whether the inverse has been calculated
## previously and if so, uses the cached version. If not, it calculates the
## inverse and uses makeCacheMatrix to store the result.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if( !is.null(i)) {
        ## The inverse has been calculated
        ## message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
