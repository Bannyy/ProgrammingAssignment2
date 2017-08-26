## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that
## stores a matrix and caches its inverse.
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  mInv<- NULL
  set<- function(y){
    x<<- y
    mInv<<- NULL
  }
  get<- function()x
  setinverse<- function(inverse) mInv<<- inverse
  getinverse<- function() mInv
  list(set= set, 
       get= get, 
       setinverse = setinverse,
       getinverse = getinverse)
  
}
## This function computes the inverse of the special "matrix" created by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mInv<- x$getinverse()
  if(!is.null(mInv)){ 
    message("getting cached data")
    return(mInv)
  }
  mat <- x$get()
  mInv <- solve(mat, ...)
  x$setinverse(mInv)
  mInv
  
}
