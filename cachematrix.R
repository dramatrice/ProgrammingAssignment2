## These functions create a list of functionsand values that
## are then cached for later reuse.

## This function creates the initial list of functions to set 
## the value of a matrix, return it, as well as set
## and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function () x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set=set, get=get, 
         setsolve=setsolve, 
         getsolve=getsolve)
}


## This function calculates the inverse matrix only if the
## value above is NULL.  If so, it gets the value from the
## cache instead.


cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message ("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setsolve (m)
    m
        
}
