## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function merely creates returns a list of functions
# attempting to emulate the object oriented programming pattern of
# encapsulating data and exposing them through getters/setters
# Here a setter and getter function is initialized for the matrix
# and it's potentially cached inverse value. The inverse matrix is set
# to NULL until it is computed lazily. Also important to note is the
# setter of the matrix also sets the cached inverse matrix to NULL
# to ensure that the cache does not store invalid data.
# Global variables are used to store the matrix and it's cached inverse
# in 'x' and 'inv' respectively which enables the cache retrieval function
# to access this variable outside of makeCacheMatrix, this is achieved 
# using the super assignment operator <<-
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
# Given the list of getters/setters created by the 'makeCacheMatrix'
# function above, this function attempts to get the cached value of
# the inverse matrix. A null check detects if the inverse have been
# computed before and returns it immediately if there is a value.
# If the inverse value is null, the getter is called to retrieve the
# matrix and it is passed to the 'solve' function to compute its 
# inverse. The computed inverse is subsequently stored in the cached
# 'inv' variable before being returned.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
