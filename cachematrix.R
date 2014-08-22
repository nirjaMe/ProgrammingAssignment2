## Assignment: Caching the Inverse of a Matrix
## This script contins two functions namely:
## makeCacheMatrix and cacheSolve

## Assumption: The matrix supplied is always invertible
## To use this script, myMatrix <- makeCacheMatrix(matrix(ENTER MATRIX DATA HERE))
##                     cacheSolve(myMatrix)

## This function creates a special "matrix" object that can cache its inverse.
## Input parameter: Matrix
makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        set <- function(y = matrix()){
                x <<- y
                invMatrix <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) invMatrix <<- inv
        getInverse <- function()invMatrix 
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



##Function to compute and retrieve the inverse of supplied matrix
## It will return cached data, if the inverse is already calculated
## otherwise, will calcuate the inverse
## Output parameter: Inverse of the supplied matrix

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
            if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
            }
        data <- x$get()
        inv <- solve(data)
        x$setInverse(inv)
        inv
}