## This file contains two functions:
## 1. makeCacheMatrix: creates a special matrix object that can store its inverse.
## 2. cacheSolve: computes the inverse of the matrix returned by makeCacheMatrix.
##    If the inverse has already been calculated, it retrieves it from the cache
##    instead of recomputing it.

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL   # This will store the cached inverse
  
  # Function to set a new matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL   # Reset cached inverse when matrix changes
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getInverse <- function() inv
  
  # Return all functions as a list
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  
  # Try to get cached inverse
  inv <- x$getInverse()
  
  # If inverse already exists, return it
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise compute inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  # Store inverse in cache
  x$setInverse(inv)
  
  inv
}
