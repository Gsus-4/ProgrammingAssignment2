## This pair of functions calculate and store in cache the inverse of a given matrix x, supposed invertible

## The following one stores the given matrix and its inverse in a list object:

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
        x <<- y
        inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following one checks if the inverse has already been calculated and returns the cached matrix if so. If not, it calculates the inverse matrix.
## The input must be the list previously generated with the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if( !is.null(inv) ) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
    
    
}
