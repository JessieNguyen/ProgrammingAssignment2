## functions to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse_mat <- NULL
  set <-function(y) {
    x <<- y
    inverse_mat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) {
    inverse_mat <<-inverse  
  }
  getinverse <- function() inverse_mat
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  inverse_mat <-x$getinverse()
  if (!is.null(inverse_mat)){
    message("getting cached data")
    return(inverse_mat)
  }
  data <- x$get()
  inverse_mat <- solve(data)
  x$setinverse(inverse_mat)
  inverse_mat
}