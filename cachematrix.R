## This function creates a special "matrix" object that can cache its inverse.
## Input of makeCacheMatrix an invertible matrix. 
## It must be used combined with the cacheSolve function, which will compute
## an object storing the makeCacheMatrix and return the respective
## inverse matrix.
## For further information, see also description of cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
    ## Declares the variable "inverse", that will store the
    ## inverse of the matrix "x".
    inverse <- NULL
    
    ## Substitutes the matrix "x", stored in the main function, with "y".
    ## It also restores to null the value of the "inverse", because the
    ## old inverse of the old matrix is not needed anymore.
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    ## Returns the matrix "x" stored in the main function.
    ## Doesn't require any input.
    get <- function() x
    
    ## Stores the value of the input in a variable "inverse" 
    ## into the main function makeCacheMatrix
    setinverse <- function(newinverse) inverse <<- newinverse
    
    ## Returns the value of the variable "inverse" 
    ## of the main function makeCacheMatrix
    getinverse <- function() inverse
    
    ## Stores the 4 functions in the function makeCacheMatrix.
    ## When makeCacheMatrix is assigned to an object, the object 
    ## has all the 4 functions (set, get, setinverse and getinverse).
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes and returns the inverse of the special "matrix" 
## returned by makeCacheMatrix function.
## Input of cacheSolve is the object where makeCacheMatrix is stored.
## Example:
##  > a <- makeCacheMatrix(x <- matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))
##  > cacheSolve (a)

cacheSolve <- function(x, ...) {
    ## Stores the value of the inverse of the matrix passed as argument
    ## to the function makeCacheMatrix
    inverse <- x$getinverse()
    
    ## Verifies the value "inverse" exists and is not NULL. 
    ## If it exists in memory, it simply returns a message and the "inverse".
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## "Data" gets the matrix stored with makeCacheMatrix,
    ## "inverse" calculates the inverse of the matrix (using solve() function) 
    ## and x$setinverse(inverse) stores it in the object generated 
    ## assigned with makeCacheMatrix.
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
