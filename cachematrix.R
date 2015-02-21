## The purpose of the following two functions is to reduce the number of 
## times an inverse matrix must be calculated by caching the inverse after
## the first calculation and returning the cached matrix if requested 
## in a later calculation.  This follows almost exactly the Mean example except
## we are using a Matrix instead a Vector for the calculation input and we are 
## using Solve function to calcualte the Inverse Matrix instead of the Mean 
## function.

## "makeCacheMatix" returns a list of 4 functions that 'set' or 'get' the input 
## matrix and 'setinvmatrix' to hold the inverse matrix or 'getinvmatrix' 
## assuming it resides in memory.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvmatrix <- function(solve) m<<- solve
    getinvmatrix <- function() m
    list(set = set, get = get,
         setinvmatrix = setinvmatrix,
         getinvmatrix = getinvmatrix)    
}

## "cacheSolve" first attempts to get the inverse matrix.  If a valid value is
## returned, then a message displays that the cached value was retrieved and 
## is displayed.  If the 'm' matrix space is NULL, then the function will get
## the matrix, calculate the inverse matrix using Solve, set the inverse matrix
## in memory and return the inverse matrix in 'm'. 

cacheSolve <- function(x = matrix(), ...) {
    m <- x$getinvmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setinvmatrix(m)
    m
}