## These functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## The inverse starts out as NULL
  ## By setting the object, I assign the new argument to be my stored value and
  ## reset my inverse calculation
  set <- function(y) {
    x <<- y
    matrix_inv <<- NULL
  }
  get <- function() x
  ## Get and Set functions for the inverse, as above
  setInverse <- function(inverse) matrix_inv <<- inverse
  getInverse <- function() matrix_inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been 
##calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  matrix_inv <- x$getInverse()
  if(!is.null(matrix_inv)) {
    message("getting cached data")
    return(matrix_inv)
  }
  data <- x$get()
  inv <- solve(data) ## calculate the inverse
  x$setInverse(matrix_inv)
  matrix_inv
}
