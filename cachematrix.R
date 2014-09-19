## These functions are responsible for calculate and cache the inverse 
## of a matrix

## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.
## input: a matrix
makeCacheMatrix <- function(x = matrix()) {
        # variable that holds the inverse matrix
        m <- NULL
        # set a new matrix
        set <- function(y) {
                x <<- y
                # void the inverse matrix
                m <<- NULL
        }
        # return the last matrix set
        get <- function() x
        # set a new value for the inverse
        setsolve <- function(solve) m <<- solve
        # return the inverse
        getsolve <- function() m
        # list with functions to be called
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## cacheSolve
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache.
## input: a matrix created with the function makeCacheMatrix
cacheSolve <- function(x, ...) {
        # variable that holds the inverse matrix
        m <- x$getsolve()
        if(!is.null(m)) {
                # the inverse matrix was calculated before
                message("getting cached data")
                # Return a matrix that is the inverse of 'x'
                return(m)
        }
        
        data <- x$get()
        # calculate the inverse
        m <- solve(data, ...)
        x$setsolve(m)
        # return a matrix that is the inverse of 'x'
        m
}
