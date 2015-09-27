## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setM <- function(y) {
                x <<- y
                inv <<- NULL
        }
        getM <- function() x
        
        # store matrix
        setInverse <- function(inverse) inv <<- inverse
        
        # return the sotred matrix
        getInverse <- function() inv
        
        # return the list with each function name to the element
        list(setM = setM,
             getM = getM,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        # return the value if it exists
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setInverse(inv)
        
        # return the inverse result
        inv
}


