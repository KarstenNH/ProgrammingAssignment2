# makeCacheMatrix: 
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # makeCacheMatrix:
  # creates a special "matrix", which is really a list containing a function to
  # set the value of the matrix
  # get the value of the matrix
  # set the inverse of the matrix
  # get the inverse of the matrix
  invMatrix = NULL
  setMatrix = function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix = function() x
  setinvMatrix = function(inverse) invMatrix <<- inverse 
  getinvMatrix = function() invMatrix
  list(setMatrix=setMatrix, getMatrix=getMatrix, setinvMatrix=setinvMatrix, getinvMatrix=getinvMatrix)
}

# cacheSolve: 
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should 
# retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  # cacheSolve:
  # returns the inverse of the matrix input to makeCacheMatrix()
  invMatrix <- x$getinvMatrix()
  
  # if the inverse exists..Cache
  if (!is.null(invMatrix)){
    message("getting cached data")
    return(invMatrix)
  }
  
  # Calculate the inverse 
  mat.data <- x$getMatrix()
  invMatrix = solve(mat.data, ...)
  
  # sets the value of the inverse in the cache.
  x$setinv(invMatrix)
  
  return(invMatrix)
}








