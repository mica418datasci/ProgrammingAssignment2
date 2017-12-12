## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## MakeCacheMatrix returns a 4-member list. Each member is a reference to a function that
## 	1. returns a matrix (get), 
## 	2. sets the new matrix (set) and sets the value of the m (inverse matrix) to NULL, 
## 	3. returns an inverse matrix (getinv), 
## 	4. sets the inverse matrix (setinv) 


 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL ## The matrix has changed, so there will be a need to recompute the inverse matrix. 
			   ## Othervise, there would be a value in m, and cacheSolve function would return it instead to
			   ## recompute the inverse matrix again... 
  }
  get <- function() x
  setinv <- function(invm) m <<- invm
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## To get the inverse of the matrix, we use the cacheSolve function
## cacheSolve function first evaluates is the variable m NULL. If NULL, this means that the 
## matrix has changed and that the computation has to be done on the changed matrix. If not NULL,
## this means that the there is no need to recompute the inverse matrix and that the cached inverse
## matrix can be returned. 


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()     
  if(!is.null(m)) {  ## this line evaluates is there a need to recompute the inverse matrix. 
    message("getting cached matrix data")
    return(m)  ## no need to recompute since the matrix was not changed
  }
  data <- x$get() ## matrix has changed so the inverse needs to be recomputed. 
  m <- solve(data, ...)
  x$setinv(m)  ## setting the new recomputed inverse matrix
  m
}
