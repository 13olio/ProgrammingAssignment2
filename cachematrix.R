### Caching the Inverse of a Matrix

## The computational cost of calculating the inverse of a matrix can
## be expensive.  Caching is a method of storing an object temporarily
## and recalling it rather than recalculating the object.  Caching helps
## with performance of your code.  This script contains two functions 
## (makeCacheMatrix and cacheSolve) that allow the caching of the inverse
## of a matrix.


### makeCacheMatrix function

## This function returns a list of four functions that:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  
  set <- function(y){
          x <<- y
          m <<- NULL}
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


### cacheSolve function

## This function solves for the matrix inverse from the list returned by
## makeCacheMatrix.  If the inverse has already been cached, the cached value 
## is retrieved and returned.  Otherwise, the inverse is calculated and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
    
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
