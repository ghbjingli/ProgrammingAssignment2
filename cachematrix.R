## Below are two functions that are used to create a 
## special object that stores a matrix and caches its inverse.

## Function makeCacheMatrix creates a special "matrix", 
## which is really a list of functions associated with the matrix
## set - set the matrix
## get - get the matrix
## setinv - set inverse matrix
## getinv - get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Fuction cacheSolve calculates the inverse of the special matrix created with the
##  makeCacheMatrix function. It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates 
## the inverse of the data and sets the value of the inverse in the cache via the setinv function

cacheSolve <- function(x, ...) {
        inv<- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
