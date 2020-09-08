## These functions serve to first create an object that stores 
## a matrix and then caches the inverse of that matrix

## The makeCacheMatrix creates an object that contains a
## matrix with a list of functions that set and get the
## matrix contents, and set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv, 
         getinv = getinv)
}


## The cacheSolve function first checks to see if the inverse
## of the matrix created from makeCacheMatrix has been calculated.
## If the matrix inverse is already cached it will return it,
## and if not, then it will calculate the inverse

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
