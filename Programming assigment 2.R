# makeCacheMatrix creates a list of functions with which to investigate a matrix object

#Function to create list of functions for matrix investigation.
#ARGS: x -> matrix object, DEFAUlT: matrix()
#RETURNS: list of functions:
#                            set(newMatrix) : sets matrix
#                            get() : returns matrix object
#                            getInverse() : returns matrix inverse
#                            setInverse(matrixInverse) : sets matrix inverse  
makeCacheMatrix <- function(x = matrix()) 
{
  
  inverse = NULL
  
  set <- function(newMatrix)
  {
    x <<- newMatrix
    inverse = NULL
  }
  
  get <- function()
  {
    x
  }
  
  setInverse <- function(matrixInverse)
  {
    inverse <<- matrixInverse
  }
  
  getInverse <- function()
  {
    inverse
  }
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

# cacheSolve returns a matrix inverse. If the inverse has not been calculated previously, it is checked for existance (square and
# invertible) and solved for.
# The result is then stored. If the inverse has been solved for previously, the inverse is merely retrieved from the cache.

#Function to cache and return matrix inverse.
#ARGS: x -> matrix object
#      ... -> additional args
#RETURNS: matrix inverse

cacheSolve <- function(x, ...) 
{
  
  xInverse <- x$getInverse() 
  
  if(!is.null(xInverse))
  {
    message("Getting cached data")
    return(xInverse)
  }
  
  matrix_x <- x$get()
  
  #Ensure matrix is square
  if(dim(matrix_x)[1] != dim(matrix_x)[2])
  {
    print("Matrix not square!")
    return(xInverse)
  }
  #Ensure matrix is invertible
  else if (det(matrix_x) == 0)
  {
    print("Matrix not invertible!")
    return(xInverse)
  }
  else
  {
    xInverse <- solve(matrix_x)
  }
  
  x$setInverse(xInverse)
  
  xInverse
}
