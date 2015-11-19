## This file contains two functions:
## The makeCacheMatrix, which creates a list containing the matrix information 
## plus functions that allow caching of the matrix.
## The cacheSolve function which allows to cache the calculation of the inverse
## of the created matrix.

##example Matrix 
# matrix(c(1,2,3,4),nrow =2,ncol=2)
# with its inverse matrix(c(-2,1,1.5,-0.5),nrow=2, ncol=2)

##Function one:
# Creates a list with four elements.
# 1. "set": a function to save the matrix (in a different function environment!)
# 2. "get": to get the matrix
# 3. "setsolve": a function to calculate the inverse of the matrix
# 4. "getsolve": a function to retrieve the inverse of the calculated matrix
## Attention: The variables are set in their function environment.

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function two:
# Returns the inverse of the input matrix. Before calculating the inverse, it is 
# checked if the inverse has already been calculated.  see below

        
cacheSolve <- function(x,...){
    # Check if inverse is already calculated (getsolve), if this is so return s with message
    s <- x$getsolve()
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    
    }
    # If inverse has not yet calculated ...
    data <- x$get() #...get matrix stored in x$get
    s <- solve(data, ...) # ..calculate its inverse
    x$setsolve(s) #.. store the inverse for future caching
    s #... and return the calculated inverse
  }
  

