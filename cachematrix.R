## Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse
## of a matrix rather than computing it repeatedly.
## These functions in this file cache the inverse of a matrix.

## makeCacheMatrix receives a matrix x as a parameter
## (assumed to be invertible),
## and creates a "special" matrix, which is a list
## containing functions to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)    
}


## cacheSolve calculates the inverse of the "special"
## matrix created with makeCacheMatrix. It first checks
## if the inverse of the matrix has already been calculated.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
