## This file contains code for the creation of two fuctions...
# 1). makeCacheMatrix
# 2). cacheSolve

## makeCacheMatrix:creates a special "matrix" object that can cache its inverse;
#contains a function to 
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse
#4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) { 
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) inv <<- solve
    getsolve <- function() inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


#Cachesolve: computes the inverse of the "matrix" returned by makeCacheMatrix(). 
#If the inverse has already been calculated and the matrix has not changed, 
#the function retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
    inv <- x$getsolve()x
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
}
