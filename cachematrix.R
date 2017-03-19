## The following functions provides the possibility to keep 
## the result of the inverse of a matrix in cache. This is to avoid the
## calculation of the inverse each time we need to apply that operation

## makeCacheMatrix creates an special matrix with properties and operations
## Returns a list with the operations that will be available for this type of
## matrix.
## Properties
##          x: the matrix of values to be evaluated
##          cacheInv: cached result of inverse of the matrix
## Operations
##          set: sets a new matrix of values
##          get: gets the current matrix of values
##          setInv: sets the value of the cached result
##          getInv: gets the current cached inverse result

makeCacheMatrix <- function(x = matrix()) {
    cacheInv <- NULL
    set <- function(y) {
        x <<- y
        cacheInv <<- NULL
    }
    get <- function() x
    setInv <- function(inv) cacheInv <<- inv
    getInv <- function() cacheInv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve applies the function solve() to the special matrix sent in the parameters
## if the special matrix already has an inverse calculated and has not changed, the cached
## result will be returned. 
## x: has to be a special matrix created with the make makeCacheMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("Getting cached inverse data...")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}
