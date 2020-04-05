##These functions compute the inverse of an input matrix or return an already computed one
##in order to win computation time.

## This function caches an input matrix using a list with options to change it, get it, set its inverse or get its inverse 

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


## This function uses a cached matrix and, if it has never been computed, compute its
## inverse, or else, return its already computed inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
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
