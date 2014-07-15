## makeCacheMatrix and cacheSolve can be used together to cache the inverse of a matrix
## for fast retrieval

## makeCacheMatrix wraps set, get, set-inverse, get-inverse operations on a matrix
## to support caching its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## x is the matrix, inv is its inverse
  inv <- NULL
  
  set <- function(y) {
    ## set x to be a new matrix (y)
    x <<- y
    ## invalidate the previously stored matrix inverse
    inv <<- NULL
  }
  
  ## get the currently stored matrix
  get <- function() x
  
  ## set the inverse of the currently stored matrix
  setinv <- function(inverse) inv <<- inverse
  
  ## get the inverse of the currently stored matrix
  getinv <- function() inv
  
  ## makeCacheMatrix operations
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve calculates a matrix inverse, and store's it in a cache
## for fast retrieval later on

cacheSolve <- function(x, ...) {
  
  ## try to get the inverse of the matrix x
  inv <- x$getinv()

  ## if the inverse was cached, simply return it
  if(!is.null(inv)) {
    return(inv)
  }
  
  ## otherwise, calculate the inverse of x and store it in the cache
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  
  ## return the calculated inverse
  inv
}
