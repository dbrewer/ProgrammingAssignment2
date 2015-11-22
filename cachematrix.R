## Functions to cache a matrix, invert it and cache the result.
## Saves the inversion from being recomputed repeatedly.

## Function to get and set matrix to be inverted
## and cache the inverted result.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinversematrix <- function(inv) i <<- inv
  getinversematrix <- function() i
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## Function which determines whether or not to
## compute a new inverted matrix or read an 
## existing on from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinversematrix()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinversematrix(i)
  i
}