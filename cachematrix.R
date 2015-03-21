## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix initialises the solution to NULL, creates the getter and
## setter functions for the matrix and its solution and returns a list
## of the methods.

makeCacheMatrix <- function(x = matrix()) {
  
  # initialise matrix to NULL
  solvedMatrix <- NULL
  
  ##Getters and Setters for Matrix
  getMatrix <- function () {
    return(x)
  }
  setMatrix <- function (newMatrix) {
    ## Store new matrix
    x <<- newMatrix 
    ## As we have a new matrix the solution cannot have been calculated yet
    solvedMatrix <<- NULL 
  }
  
  ##Getters and Setters for SolvedMatrix
  getInverse <- function () {
    solvedMatrix
  }
  setInverse <- function (invertedMatrix) {
    solvedMatrix <<- invertedMatrix
  }  
  
  ##Contruct function list
  list (getMatrix=getMatrix, setMatrix=setMatrix, 
        getInverse=getInverse, setInverse=setInverse)
}


## cacheSolve is a function which returns the inverse of a matrix
## First it checks for a cached solution and if this exists, it returns it.
## Otherwise it calculates the solution, stores the result in the cache
## and returns the solution.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  ##Retrieve cached inverted matrix
  thisInverse <- x$getInverse()
  if (!is.null(thisInverse)) {
    message("getting cached data")
    ## Inverse already calculated. return cached value
    return(thisInverse)
  }
  
  ##Otherwise, retrieve data and invert
  thisMatrix <- x$getMatrix()
  thisInverse <- solve(thisMatrix, ...)
  
  ## Store result in cache
  x$setInverse(thisInverse) 
  
  return(thisInverse)
}
