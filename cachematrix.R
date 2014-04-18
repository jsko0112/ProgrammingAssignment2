## ProgrammingAssignment2
## by Alex Ko

## his function creates a special "matrix" object that can cache its inverse
## - set the value of the matrix
## - get the value of the matrix
## - set the value of the inverse
## - get the value of the inverse

makeCacheMatrix  <- function(x = matrix()) {
    
    # initialize the stored inverse value to NULL
    val <- NULL
    
    # to set the value of the matrix
    setval <- function(y) {
        x <<- y
        val <<- NULL   
    }
    
    # to get the value of the matrix
    getval <- function() x
    
    # to set the inverse for caching
    setinvmatrix <- function(invmatrix) val <<- invmatrix
    
    # to get the inverse which is cached
    getinvmatrix <- function() val
    
    # return a list of all the above functions
    list(setval = setval, getval = getval,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)    
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve  <- function(x, ...) {
    
    # check whether the inverse is already cached or not
    cinv <- x$getinvmatrix()
    if(!is.null(cinv)) {
        message("this is cached data")
        return(cinv)
    }
    
    # not cached, so we get the matrix into data
    data <- x$getval()
    
    # and compute the inverse
    cinv <- solve(data,...)
    
    # then cache the inverse
    x$setinvmatrix(cinv)
    
    # and return
    cinv
}