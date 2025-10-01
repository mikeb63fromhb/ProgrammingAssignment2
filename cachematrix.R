## These functions work together to handle matrices more efficiently.
## The first function creates a special matrix object that can store
## its value and remember its inverse. The second function calculates
## the inverse or retrieves the saved version if already available.
## This caching process saves time because it avoids repeating the
## same heavy calculation when the matrix has not changed.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function checks if the inverse of the matrix has already
## been calculated. If so, it quickly returns the saved result
## from memory. If not, it computes the inverse using the solve
## function, saves it for later use, and then returns it.
## This avoids wasting time repeating the same calculation.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
