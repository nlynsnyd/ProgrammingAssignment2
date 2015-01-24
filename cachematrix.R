## Matrix inversion is a costly computation, thereby producing some benefit by
##  caching the inverse of a matrix rather than computing it repeatedly. The 
##  following functions create a special matrix that can cache its inverse, 
##  compute this inverse, and retrieve it from the cache upon command.

## makeCacheMatrix creates a special "matrix" object that can cache
##  its inverse for time savings.

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


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse of this matrix has already been computed, then cacheSolve prints that it is
##  getting cached data and then retrieves the inverse from the cache.

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
