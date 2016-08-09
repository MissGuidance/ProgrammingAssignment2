# Caching the Inverse of a Matrix
# Both functions found below will calculate or retrieve the inverse of a 
# matrix from the cache, when given an invertible matrix

## The "makeCacheMatrix" function creates a special matrix object that can chache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get<- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
}
  

## The "cacheSolve" function uses the special matrix returned by the "makeCacheMatrix" 
## function to compute its inverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("retrieving the chached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
