## This is an implementation for caching of inverse matrix for a matrix.
## 

## makeCacheMatrix creates a special matrix in which inverse of the matrix can be cached

makeCacheMatrix <- function(x = matrix()) {
  ## inv - used to store the inverse of the matrix
  inv <- NULL     
  set <- function(y) {   ## function to set the matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x   ## function to get the matrix
  setinverse <- function (invmatrix) inv <<- invmatrix  ## function to set the inverse of the matrix
  getinverse <- function () inv  ## function to get the inverse of the matrix
  ## Returns list of functions to set a matrix, get the matrix, set the inverse of the matrix and
  ## get the inverse of the matrix  
  list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}


## cacheSolve returns the inverse of a special matrix from the cache or it sets the inverse matrix
## to the cache

cacheSolve <- function(x, ...) {
  invm <- x$getinverse()
  if (!is.null(invm)){
    print("Getting the inverse of matrix from Cache")
    return(invm)  ## Return a matrix that is the inverse of 'x' from cache
  }
  mat <- x$get()
  invm <- solve(mat,...)  ## Forming the inverse of matrix 'x'
  x$setinverse(invm)    ## Set the inverse of matrix in Cache
  invm   ## Return a matrix that is the inverse of 'x'
}
