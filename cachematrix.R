## Put comments here that give an overall description of what your
## functions do

library(matlib) ## Using matlib package for inv() function.
## makeCacheMatrix creates a special object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        get <- function() x
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get, 
        setinv = setinv, 
        getinv = getinv)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
  }
  data <- x$get()
  i <- inv(data, ...)
  x$setinv(i)
  i        ## Return a matrix that is the inverse of 'x'
}
