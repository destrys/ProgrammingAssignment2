## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix - Creates a list of functions for the following matrix functions:
##   set - set the value of the matrix
##   get - get the value of the matrix
##   setSolve - set the value of the inverse
##   getSolve - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() s
  list(set = set, get = get, 
       setSolve = setSolve, 
       getSolve = getSolve)
}


## cacheSolve - operates on a makeCacheMatrix operation to either
##   compute the inverse of the matrix and cache it, or return the 
##   already cached inverse.

cacheSolve <- function(x, ...) {
  s <- x$getSolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setSolve(s)
  s
}
