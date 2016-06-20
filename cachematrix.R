## This file contains two functions:
## - makeCacheMatrix: This function creates a special "matrix" object that can 
##   cache its inverse;
## - cacheSolve: This function computes the inverse of the special "matrix" returned
##   by makeCacheMatrix. If the inverse has already been calculated,
##   then the cachesolve retrieves the inverse from the cache.



## makeCacheMatrix creates a special "matrix" object suitable for efficient
## inverse calculation. The function returns a list with functions to
## set and get the data of the special "matrix" and its inverse.
## makeCacheMatrix throws an error if the value set or passed to the function 
## is not a matrix of numeric values.

makeCacheMatrix <- function(x = matrix()) {

       stopifnot(is.matrix(x) && is.numeric(x))
  
       inv <- NULL
       
       set <- function(y) {
         
         stopifnot(is.matrix(y) && is.numeric(y))
         
         x <<- y
         inv <<- NULL
       }
       
       get <- function() x
       
       setInverse = function(inverse) inv <<- inverse
       
       getInverse <- function() inv
       
       list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve accepts a special matrix created by invoking makeCacheMatrix.
## It efficiantely calculates the inverse of "matrix" returned by makeCacheMatrix.
## Precondition: the "matrix" returned by makeCacheMatrix is invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
       inv <- x$getInverse()
       
       if (!is.null(inv)) {
         message("getting cached inverse")
         return(inv)
       }
       
       m <- x$get()
       
       inv <- solve(m, ...)
       
       x$setInverse(inv)
       
       inv
}
