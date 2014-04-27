## These functions cache the inverse of a matrix, in order to save time on a potentially
## costly computation.

## makeCacheMatrix creates a special object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL    
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheMatrix computes the inverse of the object returned by makeCacheMatrix.
## If the inverse has already been calculated for the same matrix, cacheSolve will retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
