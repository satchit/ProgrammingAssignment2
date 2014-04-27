## Creates a function that caches the result of
## calculating the inverse of a matrix


## This function does the work of storing and returning the cached result

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    y <- x
    get <- function() y
    set <- function(x) {
        y <<- x
        inv <<- NULL
    }
    getInverse <- function() inv
    setInverse <- function(i) {
        inv <<- i
    }
    list(get=get, 
         set=set, 
         getInverse=getInverse, 
         setInverse=setInverse)
}


## Uses the makeCacheMatrix function to return a cached result
## or calculate a new one and cache that

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("Inverse already calculated. Returning cached value")
        return (inv)
    }
    data <- x$get()
    inv <- solve(data,...)
    x$setInverse(inv)
    inv
}
