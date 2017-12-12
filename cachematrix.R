##this experiment is to use and understand a pair of functions, which are,
##makeCacheMatrix" and "cacheSolve" that cache the data of the inverse of a matrix.

## makeCacheMatrix is a function which creates a special "matrix" object that can 
## cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve should retrieve the 
##inverse from the cache.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  result <- x$get()
  inv <- solve(result, ...)
  x$setinv(inv)
  inv
}
