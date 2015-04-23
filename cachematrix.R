## The functions calculate the inverse of a matrix, and if the inverse was already
## calculated, it returns the stored inverse instead of calculating again.

## function makeCacheMatrix assigns value to a matrix or resets the value of 
## a matrix. It also sets the rule of retriving the value of the inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinv <- function (inverse) inv <<- inverse
      getinv <- function () inv
      list(set=set, get=get,
           setinv=setinv,
           getinv=getinv)
}


## function cacheSolve verifies if the inverse already exists. If so it retrieves
## the stored value instead of recalculating; otherwise it calculates the value.
cacheSolve <- function(x, ...) {
      inv <- x$getinv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinv(inv)
      inv## Return a matrix that is the inverse of 'x'
}
