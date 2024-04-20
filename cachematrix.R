## Two functions ("makeCacheMatrix" and "cacheSolve") that cache the inverse
## of a matrix

## The "makeCacheMatrix" function creates a unique matrix object that is able
## to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(matrix) {
           x <<- matrix
           inv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) inv <-- inverse
    
    getInverse <- function() inv
    
    list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## The "cacheSolve" function returns the inverse of the matrix that is 
## created by the "makeCacheMatrix" above. The "cacheSolve" function 
## should return the inverse from the cache of the "makeCacheMatrix" if
## the inverse of the matrix returned above is already calculated.

cacheSolve <- function(y, ...) {
        x <- y$getInverse()
        
        if(!is.null(x)) {
            message("retrieving cached data")
            return(x)
        }
        
        data <- y$get()
        
        x <- solve(data, ...) 
        
        y$setInverse(x)
        x
}
