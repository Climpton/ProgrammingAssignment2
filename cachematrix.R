## First function (makeCacheMatrix) creates a matrix object. Second function 
## (cacheSolve) calculates the inverse of a matrix and updates the matrix object.

## The makeCacheMatrix contains the following functions:
## set - Sets the value of the matrix
## get - Gets the value of the matrix
## setinv - Sets the value of the matrix inverse
## getinv - Gets the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## If the matrix inverse is calculated return the value. If not calculate and return 
## the matrix inverse and update the matrix object.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("Getting cached data")
        return(i)
    }
    mat <- x$get()
    i <- solve(mat)
    x$setinv(i)
    return(i)
}
