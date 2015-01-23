## Function makeCacheMatrix can be used to create a special matrix
## that is capable of caching its inverse matrix when it is solved
## by calling cacheSolve function on it.

## Creates a special matrix which is capable of caching its inverse.
## Use $get funtion to retrieve the original matrix value.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of matrix x from its cache or calculates 
## the inverse, caches it for next calls and returns the inverse.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
