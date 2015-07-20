## The following creates cachable matrices. Given a matrix, when inversed, then store it in cache.
## Retrieving an inversed matrix will check the cache first. If it exists in the cache, retrieve it, otherwise, create a new inverse for the matrix.

## create a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    message("generating matrix")
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Retrieve the inverse in the cache, if it exists.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    message("retrieving matrix")
    data <- x$get()
    message("generating inverse")
    i <- solve(data, ...)
    x$setInverse(i)
    i    
}
