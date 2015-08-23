## Put comments here that give an overall description of what your
## functions do
##
## These functions use lexical scoping to cache the value of the
##   inverse of a matrix, rather than needing to recalculate the
##   inverse over and over. 
##

## Write a short comment describing this function
##   This function sets up the infrastructure and helper functions
##   to keep our inverse matrix cached.
makeCacheMatrix <- function(x = matrix()) {
  # initialize the inverse matrix
  inv <- NULL
  
  # function to set the input matrix and its inverse
  # note that whenver we reset the input matrix,
  # we must reset the inverse
  set <- function(y) {
    x <<- y  # we use the superassignment <<- operator
    inv <<- NULL # to keep these in the global environment 
  }
  
  # function to return the input matrix
  get <- function() x
  
  # function to set the inverse matrix
  setinv <- function(inverse) inv <<- inverse
  
  # function to return the inverse matrix
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## Write a short comment describing this function
## This function returns the inverse of the matrix 'x'
##   After the initial computation, it returns a cached
##   version of the inverse.
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## First we check if the inverse has already been cached
  inv <- x$getinv()
  if(!is.null(inv)) {
    # Yes, inv exists already, so let's use it and we're done
    message("getting cached data")
    return(inv)
  }
  
  ## The inverse has not been cached so calculate it
  data <- x$get()  # data is a copy of the input matrix
  inv <- solve(data, ...) # invert the matrix
  x$setinv(inv) # save the cached copy 
  inv    # return the calculated inverse
}
