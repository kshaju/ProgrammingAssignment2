## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function (invmatrix) inv <<- invmatrix
  getinverse <- function () inv
  list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invm <- x$getinverse()
  if (!is.null(invm)){
    print("Getting the inverse of matrix from Cache")
    return(invm)
  }
  mat <- x$get()
  invm <- solve(mat,...)
  x$setinverse(invm)
  invm
}
