## This file contains 2 functions
## makeCacheMatrix - create a special matrix object
## cacheSolve - returns inverse of matrix, and 
##              returns the inverse from cache if the same matrix has been calculated.


## makeCacheMatrix 
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
 
  # set the value of given matrix
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # get the value of the matrix
  get <- function() x
  
  # set the value of the inverse
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv

  # get the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## CacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then
## then cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  # check for previously run matrix
  inv <- x$getinverse()
  
  # check if cacheSolve run before
  if (!is.null(inv)) {
    message("getting inverse cached data")
    return(inv)
  }
  
  # get inverse of given matrix
  data <- x$get()
  inv <- solve(data)
  
  # set inverse of given matrix
  x$setinverse(inv)
  inv
}
