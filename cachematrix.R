## Put comments here that give an overall description of what your
## functions do

## x is an invertible matrix
## makeCacheMatrix is a list containing functions:
##  - one set function to store the matrix in an environment different from current
##  - one get function to get the matrix from the other environment
##  - one set function to set the inverse of x in an environment different form current
##  - one get function to get the inverse of x from the other environment
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## x is the return value of the makeCachedMatrix
cacheSolve <- function(x, ...) {
    ## get the inverse of x sotred in a different environment from current
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## if the inverse is not cached or the matrix has changed
    ## then retrieve matrix, calculate the inverse and then store it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
