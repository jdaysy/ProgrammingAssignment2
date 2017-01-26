## These functions allow the inverse of a matrix to be saved to cache and 
## retrieved.

## makeCacheMatrix makes a list of functions to be able to set/get the value of
## a matrix and get/set the value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
            m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setsolve <- function(solve) m <<- solve
            getsolve <- function() m
            list(set = set, get = get,
                 setsolve = setsolve,
                 getsolve = getsolve)
}


## cacheSolve takes the resultant list of functions from makeCacheMatrix and 
## returns the inverse of the matrix from cache if available (and the matrix is 
## unchanged), or by calculating it using the setsolve function if not.

cacheSolve <- function(x, ...) {
            m <- x$getsolve()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }
            data <- x$get()
            m <- solve(data, ...)
            x$setsolve(m)
            m
}
