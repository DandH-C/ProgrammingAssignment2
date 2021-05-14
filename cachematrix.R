## Functions to cache potentially time-consuming computations.
## The makeCacheMatrix will create a matrix that cache its inverse. 
## CacheSolve does 2 things: If the inversed matrix has already been calculated (and no change to the matrix) 
## this function will retrieve the inverse from the cache otherwise will do the computations.


## As described earlier, this function create an inversed cache matrix

makeCacheMatrix <- function(x = matrix()) {
    
  ## creating inv variable to hold the inversed matrix  
      inv <- NULL
      
      set <- function(y) {
          x <<- y
          inv <<- NULL
        
      }
      get <- function() x
          setinverse <- function(inverse) inv <<- inverse
          getinverse <- function() inv
          list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function retrieves a cache matrix and returns its inversed.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    inv <- x$getinverse()
      if(!is.null(inv)) {
            message("Getting cached data")
            return(inv)
      }
      
    dat <- x$get()
    inv <- solve(dat, ...)
    x$setinverse(inv)
    inv
}
