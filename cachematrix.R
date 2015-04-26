## The file contains functions which allow to cache computation of inverse matrix

## makeCacheMatrix function creates a special matrix 
## it is a list that contains 4 functions
## 1. set - set the value of the matrix
## 2. get - get the value of the matrix
## 3. setinverse - set the value of the inverse matrix
## 4. getinverse - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(y) inv <<- y
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function calculates an inverse matrix of 
## special matrix created by makeCacheMatrix
## If matrix contains cached value of inverse matrix, 
## cached value is returned. 
## If matrix doesn't contain cached value of inverse matrix,
## inverse matrix is calculated, cashed and returned.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
