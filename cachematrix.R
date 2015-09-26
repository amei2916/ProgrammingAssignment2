## Cached matrix inversion functions which stores the
## result of a matrix inversion calculation for future
## access without need of re-calculating

## Creates a special matrix which caches its inverse
## (based on makeVector example)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv,
    getinv = getinv)
}


## Cached matrix inversion which checks if the inverse
## of a matrix has previously been calculated
## (based on cachemean example)

cacheSolve <- function(x, ...) {
  inv <- x$getinv() # get cached inverse
  # returns cached data if present
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  # calculates inverse if cached data not present
  data <- x$get()
  inv <- solve(x, ...)
  x$setinv(inv)
  inv
}
