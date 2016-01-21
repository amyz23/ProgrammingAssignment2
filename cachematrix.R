## 
## Functions utilizing caching to calculate Matrix inversions
##

## makeCacheMatrix
## This function creates a matrix "object" used to cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  inv_set <- function(solve) minv <<- solve
  inv_get <- function(solve) minv
  list(set = set, get = get,
       inv_set = inv_set,
       inv_get = inv_get)
}


## cacheSolve
## This function computes the inversion of a square matrix based on matrix created in makeCacheMatrix 
## 
##    If inverse has been calculated and value has NOT changed
##       Use cached matrix and display notification message
##    else
##       Calculate new inversion

cacheSolve <- function(x, ...) {
  minv <- x$inv_get()
  if(!is.null(minv)) {
    message("Getting cached data")
    return(minv)
  }
  matrix <- x$get()
  minv <- solve(matrix, ...)
  x$inv_set(minv)
  return(minv)
}