## Matrix inversion is usually a costly computation and there may be
## some benefit to cache the inverse of a matrix rather than compute 
## it repeatedly.

## This function creates a special "matrix" object that can cache 
## its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ## set function. When new value is given, inv is set to null
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invertedMatrix) inv <<- invertedMatrix
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  ## if matrix inverse exists, then return cache
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## no inverse existed, calculate and save cache
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setinv(inv)
  inv
}
