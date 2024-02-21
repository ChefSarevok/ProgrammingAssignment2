## This file contains the two functions to meet the requirements for the 
## Introduction to R course, Project 2, with overall goals of calculating an
## inverse matrix and caching the results.

## This function creates a cached matrix "x", within the function "y" and 
## inverse "i".

makeCacheMatrix <- function(x = matrix()) {
  
    i <- NULL # Creation of inverse variable "i"
    set <- function(y) {
      x <<- y # Binding the open matrix argument to variable "y"
      i <<- NULL
    }
    
    get <- function() x #Caching the function "y", the matrix data, to 
    #lone function and applying inverse base function
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Runs the solve function on the matrix cached in "getinverse" applying 
## the inverse calculations. Returns inverse "x" = "i".

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
      
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
  }

