## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL;
    
    setMatrix <- function(z) {
        x <<- z
        i <<- NULL ## reset inversion state 
    }
    
    getMatrix <- function() x
    getInverse <- function() i  
    setInverse <- function(x) {
        i <<- x
    }
    
    list(setMatrix=setMatrix, 
         getMatrix=getMatrix, 
         getInverse=getInverse, 
         setInverse = setInverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##  If the inverse has already been calculated (and the matrix has not changed), 
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## check inversion   
    if (is.null(x$getInverse())) {
        print("Inverting data...")
        solvedMatrix <- solve(x$getMatrix())
        x$setInverse(solvedMatrix)
    }
    ## Return a matrix that is the inverse of 'x'
    x$getInverse()
}