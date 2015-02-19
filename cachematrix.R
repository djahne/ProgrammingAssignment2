## Author: djahne
## Create Date: 2/18/2015
## Course: R Programming
## Assignment: Programming Assignment 2
## Purpose: Provide functions to create and manipulate a CacheMatrix object, 
##  which represents a matrix with the ability to cache its inverse

## Creates a CacheMatrix object, using the input matrix as its base
## Input: x, the base matrix that will have a cacheable inverse
## Output: the CacheMatrix object, represented by a list of functions:
##  [1]: set(y) - sets the base matrix to the value of y
##  [2]: get() - returns the base matrix
##  [3]: setInverse(inverse) - sets the cached inverse value
##  [4]: getInverse() - returns the cached inverse value
makeCacheMatrix <- function(x = matrix()) {
    m_matrix <- x
    m_inverse <- NULL
    
    # function to set the base matrix
    set <- function(y) {
        m_matrix <<- y
        # set cached inverse to null initially
        m_inverse <<- NULL
    }
    
    # function to return base matrix
    get <- function() m_matrix
    
    # function to set the cached inverse value
    setInverse <- function(inverse) m_inverse <<- inverse
    
    # function to fetch the cached inverse value
    getInverse <- function() m_inverse
    
    # return list of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Returns the inverse of the input CacheMatrix using a cached value 
##  if it exists, otherwise calculating a new value and caching it for 
##  future use
## Input: x, the CacheMatrix object that will be inversed (must
##  be created with the makeCacheMatrix() function)
## Output: the inverse of the input CacheMatrix
cacheSolve <- function(x, ...) {
    # first try to fetch a cached value
    inverse <- x$getInverse()
    
    # if cached value exists return it
    if(!is.null(inverse)) {
        return(inverse)
    }
    
    # else calculate a new value
    data <- x$get()
    inverse <- solve(data, ...)
    
    # cache the new value for future use
    x$setInverse(inverse)
    
    # return the new value
    inverse
}
