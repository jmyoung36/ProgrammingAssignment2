## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a special matrix object that can calculate its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    # set inv to NULL    
    inv <- NULL
    
    # declare set function with argument y; sets x to y and inv to NULL
    # equivalent to giving new value of the matrix and removing the inverse of previous matrix
    set <- function(y) {
        
        # set value of x in PARENT environment (ie makeCacheMatrix function) to y
        x <<- y
        # set value of inv in PARENT environment (ie makeCacheMatrix function) to NULL
        inv <<- NULL
    }
    
    # declare function that returns x
    get <- function() x
    
    # declare function that takes a new inverse value and sets the value of inv in PARENT environment (ie makeCacheMatrix function) to the new inverse
    setinv <- function(inverse) inv <<- inverse
    
    # declare function that returns the inverse
    getinv <- function() inv
    
    # collect previous four functions in a list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

    # try to get a cached value of the inverse
    inv <- x$getinv()
    
    # if the cached value of the inverse is not NULL, just return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # if the cached value of the inverse is NULL, get the data
    data <- x$get()
    
    # actually calculate the inverse of the data
    inv <- solve(data, ...)
    
    # cache the newly calculated inverse of the data
    x$setinv(inv)
    
    # return the inverse
    inv
}