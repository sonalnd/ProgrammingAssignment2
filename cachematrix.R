## describes a pair of functions that create a special cache matrix
## to store the inverse of a matrix for further recall

## makeCacheMatrix uses a square matrix as an input argument
## and gives a special matrix to store its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## cacheSolve uses the output of the makeCacheMatrix as an input argument
## and checks if the inverse is already calculated. If yes, it gives a
## message and recalls the stored inverse. If not, it does the calculation.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cache value")
    return(i)
  }
  else { data <- x$get()
         i <- solve(data, ...)
         x$setinverse(i)
         i
  }
}

