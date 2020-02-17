## Coursera Course: R Programming by Johns Hopkins University

## Programming Assignment 2: Lexical Scoping
## Author: Joris Puttenstein
## Date: 17-02-2020

## Computing matrix inversion repeatedly may be an intensive procedure, 
## therefore it is desirable to caching the inverse of a matrix. 
## Below functions are found that are needed to create a special object 
## which stores the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The second function - found below - is used to cache the inverse from
## the matrix. If the inverse is already calculated, it uses that one instead.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
  }
