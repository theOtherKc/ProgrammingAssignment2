# Matrix inversion is  a costly computation.  Following functions 
# cache the inverse of a matrix instead of computing them repeatedly. 
# The functions below makeCacheMatrix and cacheSolve are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# set the value of a matrix, get the value of a matrix
# set the value of inverse of a matrix, get the value of inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


# The following function returns the inverse of the matrix. 
# It first checks if the inverse has already been computed. in this case it gets the result and skips the computation. 
# If the result has not been computed it computes the inverse and sets the value in the cache using setinverse function.

cacheSolve <- function(x, ...) {
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
