## Put comments here that give an overall description of what your
## functions do

## Create a matrix object that can cache the inverse of that matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL 
    set <- function(y)
    {
        x<<-y
        inverse <<-NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<-inv
    getInverse <- function() inverse
    list(set=set, get=get, 
         setInverse=setInverse,
         getInverse=getInverse)
}


## Returns an inverse of a matrix, utlizing a cached inverse 
## if it exists
cacheSolve <- function(x, ...) {
        inv <-x$getInverse()
        if(!is.null(inv))
        {
            message("getting cached inverse")
            return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setInverse(inv)
        inv
}
