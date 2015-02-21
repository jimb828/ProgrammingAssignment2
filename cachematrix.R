## These functions allow you to store a matrix and
## calculate its inverse. If the inverse has
## been calculated previously, it will be cached
## for reference in future calculations to
## improve performance.

## This function allows you to store a matrix
## and provides access to its inverse, if
## already calculated.
makeCacheMatrix <- function(x = matrix()) {
  ## initialize a reference to the matrix inverse
  i <- NULL
  
  ## set the value of the matrix and clear
  ## the inverse value
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## return the value of the matrix
  get <- function() x
  
  ## store the inverse of the matrix
  setinverse <- function(inv) i <<- inv
  
  ## return the inverse of the matrix
  getinverse <- function() i
  
  list(set = get, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function will return the inverse
## of a matrix stored in a CacheMatrix
## If a cached calculation exists, it will
## be returned; otherwise, it will be
## calculated and stored in the cache for
## future reference
cacheSolve <- function(x, ...) {
  ## Check if the supplied matrix cache exists
  ## and return the cached value if available
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## if a cached value doesn't exist
  ## get the matrix and solve for the inverse
  ## and then store the inverse back into
  ## the cache
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  ## Return a matrix that is the inverse of 'x'
  i
}
