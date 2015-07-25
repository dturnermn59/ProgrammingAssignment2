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
##		x 	n x n Matrix
## 
##	Output:	
##		list (set="function(y)",  
##          get="function()", 
##          setsolve="function(solve)",
##          getsolve="function()"
##         )
##
makeCacheMatrix <- function(x = matrix()) {

  ## Validate input.  Exit function if input is not a matrix.
	if (!is.matrix(x)) {
		stop("Invalid input. Expecting a matrix object.")
	}
  
  ## Only square matrices can be inverted.  
  ## Verify that the input matrix is a square matrix
  
  dm <- dim(x)
  mrow <- dm[1]
  mcol <- dm[2]
  if (mrow != mcol) {
    stop ("Invalid input.  Expecting a square matrix")
  }
  
  ## Input must be a numeric matrix
  if (!is.numeric(x[1,1])) {
    stop("Invalid input.  Expecting a numeric matrix")
  }
  
  ## Set the value of the matrix
  m <- NULL
  set <- function(y) {
            x <<- y
            m <<- NULL
  }
  
  ## Get the value of the matrix
  get <- function() x
  
  ## Set the value of matrix inverse
  setsolve <- function(solve) m <<- solve
  
  ## Get the value of the matrix inverse
  getsolve <- function() m
  
  ## Return the special "matrix" object - a list of getters and setters
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
## Return a matrix that is the inverse of x
  m <- x$getsolve()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
  
}

