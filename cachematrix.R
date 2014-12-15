## makeCacheMatrix and cacheSolve implement a system
## to cache the potentially expensive matrix inversion.

## makeCacheMatrix creates a wrapper on the given matrix x
## that can be used for various purposes, e.g. caching.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(y) inverse <<- y
  
  getinverse <- function() inverse
  
  invisible(list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse))
}


## cacheSolve calculates the inverse matrix contained in 'x' 
## and stores it in x using setinverse.
## Returns the inverse matrix.
## x is the reslut of makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  inverse = x$getinverse()
  
  if (is.null(inverse)) {
    m <- x$get()
    inverse <- solve(m, ...)
    x$setinverse(inverse)
  }
  
  inverse;
}
