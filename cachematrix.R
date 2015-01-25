## This set of functions will create an object called CacheMatrix which contains a Matrix and the Inverse Matrix cached.
## There is a function to create the object, and other to calculate the inverse and cache it for further use.
## Keep in mind that the Matrix is assumed to be invertible and square.

## I consider using the library MASS to be able to calculate the inverse matrix of square and non square matrixes,
## but according to assingment description we should assume that the matrix is invertible, that means a square matrix.
## library(MASS)
## inv <- ginv(data, ...)

## makeCacheMatrix stores the matrix and the inverse, also describes the setters and getters.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  # setter and getter for x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # setter and getter for inv
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Calculates the inverse of a matrix and caches it, if the inverse has already been calculated it will return that value instead of calculating it again.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  # we get the inverse out of the object x, if that value is not null, that means we already computed the inverse, so we return the cached value
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # if the cached value is null, we need to compute the inverse, and store it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
