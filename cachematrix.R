## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    # holds the cached value(inv) or NULL if nothing is cached
    # initially nothing is cached so set it to NULL
    inv <- NULL
    
    # store a matrix
    set <- function(y){
        
        x <<- y
        # since the matrix is assigned a new value(y), flush the inv
        inv <<- NULL
        
    }
    
    # returns the stored matrix
    get <- function() {x}
    
    # cache the given argument 
    setinv <- function(inverse) {inv <<- inverse}
    
    # get the cached(inv) value
    getinv <- function() {inv}
    
    list(set = set, get = get, 
         setinv = setinv,
         getinv = getinv)
}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix

cacheSolve <- function(z, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    # get the cached value
    mat_inv <- z$getinv()
    
    # if a cached value exists return it
    if(!is.null(mat_inv)){
        
        message("getting cached data")
        return(mat_inv)
    }
    
    # otherwise get the matrix, caclulate the inverse and store it in
    # the cache
    data<- z$get()
    mat_inv <- solve(data, ...)
    z$setinv(mat_inv)
    
    # return the inverse
    mat_inv
}
