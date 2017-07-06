## Submission for Programming Asignment 2
## Two functions that will cache the value of the inverse of a matrix,
## saving computing time

## This first function creates a special matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
      x<<- y
      inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set,
       get=get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculates the inverse of the matrix created above
## by makeCachematrix. If the inverse has already been calculated then it
##it retireves it from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cahed data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
