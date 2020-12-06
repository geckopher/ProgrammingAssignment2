## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation
## and there may be some 
## benefit to caching the inverse of a matrix
## rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(input = matrix()) {
    invert <- NULL
    set <- function(y) {
        input <<- y
        invert <<- NULL
    }
    get <- function() input
    setInverse <- function(inverse) invert <<- inverse
    getInverse <- function() invert
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This function computes the inverse
## of the special "matrix" created by 
## makeCacheMatrix above. If the inverse
## has already been calculated (and the 
## matrix has not changed), then it
## should retrieve the inverse from the cache.

cacheSolve <- function(input, ...) {
    invert <- input$getInverse()
    if (!is.null(invert)) {
        message("getting cached data")
        return(invert)
    }
    matrix <- input$get()
    invert <- solve(matrix, ...)
    input$setInverse(invert)
    invert
}
