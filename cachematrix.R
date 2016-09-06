# Matrix inversion is  a costly computation.  Following functions 
# cache the inverse of a matrix instead of computing them repeatedly. 
# The functions below makeCacheMatrix and cacheSolve are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# set the value of a matrix, get the value of a matrix
# set the value of inverse of a matrix, get the value of inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inv_x <<-inverse
    getinverse <- function() inv_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# The following function returns the inverse of the matrix. 
# It first checks if the inverse has already been computed. in this case it gets the result and skips the computation. 
# If the result has not been computed it computes the inverse and sets the value in the cache using setinv function.

cacheSolve <- function(x, ...) {
    invert <- x$getinv()
    if(!is.null(invert)) {
        message("getting cached data.")
        return(invert)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(invert)
    invert 
}
