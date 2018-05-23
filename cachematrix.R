## I have created two functions below that execute the matrix inverse operation, but stores the data within the cache
## to improve computing performance should the operation need to be executed many times.

## The first function creates cached variables for executing the inverse within the makeCacheMatrix function. In this way
## the data can be pulled from within the function environment rather than the global environment.

makeCacheMatrix <- function(x = matrix()) {
      inv_mat <- NULL
      set <- function(y) {
            x <<- y
            inv_mat <<- NULL
      }
      get <- function() {
            x
      }
      setinverse <- function(inverse) {
            inv_mat <<- inverse
      }
      getinverse <- function() {
            inv_mat
      }
      
      list(set = set, get = get, setinverse = setinverse,
                  getinverse = getinverse)
}


## The second function makes use of the predefined functions within makeCacheMatrix to pull the data and execute the inverse
## operation and spit out a result.

cacheSolve <- function(x = matrix(), ...) {
      inv_mat <- x$getinverse()
      if(!is.null(inv_mat)) {
            message("Getting cached data...")
            return(inv_mat)
      }
      data <- x$get()
      inv_mat <- solve(data, ...)
      x$setinverse(inv_mat)
      inv_mat
}
