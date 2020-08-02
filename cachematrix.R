## Function compute for the inverse of the given_matrix and cache it.

## This function gets a cache of an inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inverse
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
        
}


## This function calculates the inverse of a matrix from makeCacheMatrix function from above

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        given_matrix <- x$get()
        inv <- solve(given_matrix, ...)
        x$setInverse(inv)
        inv
}
