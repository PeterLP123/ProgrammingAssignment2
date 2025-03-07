## These functions work together to create a special matrix object that can cache its inverse.
## The makeCacheMatrix function creates a special "matrix" object that can store a matrix and its inverse.
## The cacheSolve function computes the inverse of the matrix stored in the special object.
## If the inverse has already been computed and cached, cacheSolve retrieves the inverse from the cache,
## avoiding the need to recompute it.

## Creation of a matrix object to be cached
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Returns inverse of matrix, cached if available
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
