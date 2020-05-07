
## Caching the Inverse of a Matrix:
## Matrix inversion is usually an expensive and time-consuming computation. We can cache the
## value of the inverse of a matrix to avoid repeatedly computations.
## Below are functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special matrix object that can cache its inverse.

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


## The above function computes the inverse of the special matrixcreated by 
## makeCacheMatrix. 
## Inverse will be retrieved from the cache if the inverse has already been
## calculated and the matrix has not been changed.

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

