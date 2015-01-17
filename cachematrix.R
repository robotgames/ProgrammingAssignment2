## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix
##
## This function creates a data object that stores two pieces of data,
## a matrix and its inverse.  We assume by requirement that the matrix
## is invertible.
##
## The data object this function creates provides methods to set and
## get the matrix and its inverse.  There are two important notes on
## the set and get methods.  First, when set() is used to change the matrix
## data, the inverse is set to NULL.  Second, the inverse will not be computed
## until cacheSolve is called.  At that point, cacheSolve will check
## to see whether the inverse has already been computed.  If so, the
## existing inverse is returned.  If not, the inverse is computed, stored
## in the data object, and returned.
##
## Inputs: none.  Optionally, a matrix may be passed as the initial data,
## rather than call set() later.
##
## Returns: a list.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(theinverse) m <<- theinverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
##
##cacheSolve
##
## Takes as input the data object created by makeCacheMatrix, checks
## whether the matrix inverse has already been stored.  If so, then
## the inverse is returned.  If not, the inverse is computed, stored
## in the data object, and returned.
##
## Inputs: data object created by makeCacheMatrix.  Assumes that
## the matrix stored in the data object has already been stored, and
## that the matrix is invertible.
##
## Returns: the inverse of the matrix.  Side effect is to store
## the inverse in the data object, if not already stored.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
