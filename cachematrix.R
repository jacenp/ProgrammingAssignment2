##
## The following functions allow us to compute the inverse of a square matrix, 
## and store the inverted matrix in the cache.  Subsequent calls
## to invert the matrix will return the inverted matrix that was previously
## stored in the cache as long as the matrix passed is the same as the original
## 

##
## Create a cache matrix which provides get/set functions to get and set the
## matrix, and get/set functions to get and set the inverted matrix
## 
## x: a square matrix that is invertible
##
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inverse) {
        m <<- inverse
    }
    getinverse <- function() {
        m
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##
## Calculates, or returns from the cache, the inverse of the special "matrix" 
## created with makeCacheMatrix
## 
## x: an instance of the special "matrix" created by makeCacheMatrix()
## 
cacheSolve <- function(x, ...) {
    ## Get the cached version of the inverse matrix
    m <- x$getinverse()
    
    ## If the inverted matrix exists in the cache, return it
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## If the inverted matrix isn't in the cache, retrieve the original matrix,
    ## and invert it
    data <- x$get()
    m <- solve(data, ...)
    
    ## Store the inverted matrix in the cache
    x$setinverse(m)
    
    ## Return the inverted matrix
    m
}
