## Matrix inversion is usually a costly computation. The following functions
## will allow for computing and caching matrices and their inversions.

## The `makeCacheMatrix` function allows a matrix to be set and an inverse to
## be cached along with that matrix. The function creates a listed object with
## the following functions:
##  - set: configure the matrix
##  - get: retrieve the matrix
##  - setsolve: configure the matrix inverse
##  - getsolve: retrieve the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL  # initialize inverse cache to NULL
  
  # set function to initialize or update the matrix
  # NOTE: any update will clear the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get function to retrieve the matrix
  get <- function() x
  
  # set function to initialize or update the matrix inverse
  setsolve <- function(solve) s <<- solve
  
  # get function to retrieve the matrix inverse
  getsolve <- function() s
  
  # return a list of functions
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The `cacheSolve` function will return the inverse of a matrix utilizing
## a cache from the method above to reduce calculating the inverse again

cacheSolve <- function(x, ...) {
  s <- x$getsolve()  # retrieve the matrix inverse
  
  # if inverse is cached, return it
  if(!is.null(s)) {
    message("retrieving cached data")
    return(s)
  }
  
  # inverse was not cached
  data <- x$get()  # retrieve the matrix
  s <- solve(data, ...)  # calculate the inverse
  x$setsolve(s)  # update the cache with the inverse
  s  # return the inverse
}
