## The following functions will create a matrix, calculate its inverse, and then cache it so that it does not have to be
## calculated each time it is used.

## This function creates a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
      x <<- y
      i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function will look for the inverse of the matrix created above and see if it has already cached its inverse.
## If it cannot find the inverse, then it will calculate the inverse and cache it so it does not have to be calculate
## again.

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
