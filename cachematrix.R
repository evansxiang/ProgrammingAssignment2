## makeCacheMatrix creates a matrix that can cache its inverse
## it's really just a list with 4 functions, set, get, setInverse, and getInverse
## cacheInverse returns the inverse of a matrix x using the cache if available

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv) inverseMatrix <<- inv
  
  getInverse <- function() inverseMatrix
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Returns the inverse of x, compute only if cache is not available

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
