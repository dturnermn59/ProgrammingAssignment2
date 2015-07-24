## R Programming - Assignment 2
## Debra Turner
##
## These two functions can be used together to 
## reduce the cost of matrix inversion by caching the 
## inverse of a matrix. 

## Function makeCacheMatrix
## 
##	Objective: Create a special "matrix" object that can cache its inverse.
## 
## 	Input:  
##		x	n x n Matrix
## 
##	Output:	
##		"matrix" object
##
makeCacheMatrix <- function(x = matrix()) {

  ## Validate input.  Exit function if input is not a matrix.
	if (!is.matrix(x)) {
		return("Invalid input. Expecting a matrix object.")
	}
  
  m <- NULL
  set <- function(y) {
            x <<- y
            m <<- NULL
  }
  
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
      setsolve = setsolve,
      getsolve = getsolve)

}


## Function cacheSolve
##
##	Objective: Compute the inverse of the "matrix" object created by
##		   function makeCacheMatrix. Retrieve the inverse from cache
##		   if the inverse for this matrix has already been 
##		   calculated.
##
##	Inputs:	
##		x	"matrix" object (output from function makeCacheMatrix)
##		...	additional arguments as needed
##
##	Output:	
##		Inverse of the matrix that was the input to function makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
