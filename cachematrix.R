## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL  # Initialize the inverse property
  
  ## setMatrix: Set the matrix and reset the cached inverse
  setMatrix <- function(newMatrix) {
    matrix <<- newMatrix
    inverse <<- NULL  # Reset the inverse when the matrix is set
  }
  
  ## getMatrix: Get the value of the matrix
  getMatrix <- function() matrix
  
  ## setInverse: Set the value of the inverse
  setInverse <- function(invMatrix) inverse <<- invMatrix
  
  ## getInverse: Get the value of the inverse
  getInverse <- function() inverse
  
  ## Return a list of the four functions
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(cacheMatrix, ...) {
  inverse <- cacheMatrix$getInverse()
  
  ## If the inverse is already calculated, return it
  if (!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## Otherwise, calculate the inverse
  data <- cacheMatrix$getMatrix()
  inverse <- solve(data, ...)  # Compute the inverse
  cacheMatrix$setInverse(inverse)  # Cache the inverse
  
  inverse  # Return the inverse
}
