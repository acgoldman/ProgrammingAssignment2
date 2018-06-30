## Put comments here that give an overall description of what your
## functions do

## Creates a matrix cached for use in multiple seperate calls
## By using this function, you can cache the inverse of the matrix
## this function, makeCacheMatrix merely sets the matrix and stores it in
## the calling environment rather than the function environment

makeCacheMatrix <- function(myCMatrix = matrix()) {
  i <- NULL
  set <- function(y){
    myCMatrix <<- y
    print(myCMatrix)
    i <<- NULL
  }
  get <- function() myCMatrix
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This matrix solves (calculates the inverse) of the given matrix, unless already done, in which case it returns its cached solution

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invrs <- x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  data <- x$get()
  invrs <- solve(data, ...)
  x$setinverse(invrs)
  invrs
}
