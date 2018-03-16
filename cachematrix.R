## Below are two functions that are used to create a
## special object that stores a matrix and caches its inverse.

## The function makeCacheMatrix creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    # initialize inverse to NULL
    inverse <- NULL
    
    # setter and getter for the original matrix
    set <- function(y) {
        x <<- y
        # if matrix is changed, cached inverse has to be reset to NULL
        inverse <<- NULL
    }
    get <- function() x
    
    # setter and getter for the cached inverse
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    
    # the function returns a list of the setters and getters
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # get the cached value of the inverse
    inv <- x$getinverse()
    
    # if it exists, return that value, and specify that it comes from the cache
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # otherwise, get the data and calculate the inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    # store the inverse in the cache
    x$setinverse(inv)
    
    # return the inverse matrix
    inv
}
