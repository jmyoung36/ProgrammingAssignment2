# Two functions are included in this file
# makeCacheMatrix creates a special matrix that can calculate and cache its inverse
# has function to get and set both the original matrix and the inverse
# cacheSolve returns the inverse of a matrix created by makeCacheMatrix
# from the cache if possible, by re-calculating it otherwise

# makeCacheMatrix creates a special matrix object that can calculate its inverse
makeCacheMatrix <- function(x = matrix()) {
    
    # set inv to NULL initially    
    inv <- NULL
    
    # set function with matrix argument y; sets data x to value of y
    # also resets inv to NULL to force re-computation of inv
    # as a new matrix will have (in general) a different inverse
    set <- function(y) {
        
        # set value of x in PARENT environment (ie makeCacheMatrix function) to y
        x <<- y
        # set value of inv in PARENT environment (ie makeCacheMatrix function) to NULL
        inv <<- NULL
    }
    
    # declare function that returns value of data x
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

# cacheSolve retrieves the inverse of a matrix from am object created by makeCacheMatrix 
cacheSolve <- function(x, ...) {

    # try to get a cached value of the inverse
    inv <- x$getinv()
    
    # if the cached value of the inverse is not NULL, just return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # if the cached value of the inverse is NULL, get the data matrix
    data <- x$get()
    
    # actually calculate the inverse of the data matrix
    inv <- solve(data, ...)
    
    # cache the newly calculated inverse
    x$setinv(inv)
    
    # return the newly calculated inverse
    inv
}