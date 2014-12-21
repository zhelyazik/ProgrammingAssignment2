## This file contain 2 functions for work with square matrix

## This function return special matrix, which can cache inverse of itself
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This fuction return cache of special matrix
cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)  
  i
}
