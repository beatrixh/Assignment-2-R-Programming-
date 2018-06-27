## Put comments here that give an overall description of what your
## functions do

## sets the value of the matrix, gets the value of the matrix,
## sets the value of the inverse, gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)
}


## computes the inverse of the "matrix" returned by makeCacheMatric.
##if inverse has already been calculated, retrieves inverse from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
