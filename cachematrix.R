## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse. 
## It can set matrix, get matrix, set inverse and get inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the cache matrix and initialize the first instance as null
  cacheMatrix <- null
  ## Defining the setters and getters
  set<-function(y) {
    x <<- y
    cacheMatrix <<- null
  }
  get <- function() x
  setInverse <- function(inv) cacheMatrix<-inv
  getInverse <- function() cacheMatrix
  
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## check the cache matrix
  cacheMatrix <- x$getInverse()
  ## if cache matrix is not null return it
  if(!is.null(cacheMatrix)){
    message("getting cache matrix")
    return(cacheMatrix)
  }
  ## If cache matrix is empty, get the matrix.
  ## Create it, set and update it.
  ## Return the cache matrix
  else {
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(cacheMatrix)
    cacheMatrix
  }
  
}
