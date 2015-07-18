## Put comments here that give an overall description of what your
## functions do

## Function makeCacheMatrix implements a cache for a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## set the matrix's inverse to NULL
    inverse <- NULL
    
    ## method for setting the matrix
    set <- function(arg_x) {
        x <<- arg_x
        inverse <- NULL
    }
    
    ## method for getting the matrix
    get <- function()
        x
    
    ## method for setting the inverse
    setinverse <- function(arg_inverse)
        inverse <<- arg_inverse
    
    ## method for getting the inverse
    getinverse <- function()
        inverse
    
    ## return value of this function is a list with the four methods
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function cacheSolve calculates the inverse of a matrix, using the value in cache if available
cacheSolve <- function(x) {
    ## Obtain cached inverse matrix
    inverse <- x$getinverse()
    
    ## If there was a value in the cache, then immediately return it
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## Otherwise obtain original matrix and calcule its inverse, then store it in cache
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    
    ## Return value is the inverse matrix
    inverse
}
