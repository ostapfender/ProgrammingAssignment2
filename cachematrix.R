## Put comments here that give an overall description of what your
## functions do

## 

makeCacheMatrix <- function(x = matrix()) {
    inversed <- NULL
    ## matrix initialization, inversed version is reset
    set <- function(y) {
        x <<- y
        inversed <<- NULL
    }
    ## gets original matrix
    get <- function() x
    ## sets inverted version to 'inversed' variable
    setinversed <- function(inv) inversed <<- inv
    ## gets inversed version
    getinversed <- function() inversed
    list(set = set, get = get, 
         setinversed = setinversed, getinversed = getinversed)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inversed <- x$getinversed()
    ##check if inverted version already exists, if so returns from cache
    if(!is.null(inversed)) {
        message("getting cached matrix")
        return(inversed)
    }
    matr <- x$get()
    ## calculate inversed version of the matrix
    inversed <- solve(matr)
    ## assign inversed version to 'inversed' variable
    x$setinversed(inversed)
    inversed
}
