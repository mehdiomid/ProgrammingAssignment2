## Here, a pair of functions is written to cache the inverse of a matrix
## rather than computing it repeatedly.
## The reason is that Matrix inversion is usually a costly computation
## and there are some benefits to caching it
##
##    
## The 1st function below creates a special matrix object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inversed_matrix <- NULL
  set <- function(y) {
    x <<- y
    inversed_matrix <<- NULL
  }  
  get <- function() x
  setinverse <- function(calculated) inversed_matrix <<- calculated
  getinverse <- function() inversed_matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The 2nd function below computes the inverse of the matrix returned
## by makeCacheMatrix above. If the inverse has already been calculated,
## then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        

  inversed_matrix <- x$getinverse()
  if(!is.null(inversed_matrix)) {    ## To check the availibility of inverse
    message("getting cached data")
    return(inversed_matrix)
  }
  data <- x$get()
  inversed_matrix <- solve(data, ...)
  ## solve returns a matrix that is the inverse of our matrix
  x$setinverse(inversed_matrix) ## set the inversed matrix
  inversed_matrix
}