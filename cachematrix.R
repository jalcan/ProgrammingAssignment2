## Functions to solve and cache the inverse of a matrix. 

## The script contains two functions: 
## makeCacheMatrix: main function to hold both the matrix and its inverse
## cacheSolv: function to compute the inverse of the matrix and to cache its value via makeCacheMatrix



## Function that offers a vector-like object with:
# cached storage for a matrix and its inverse. Once the inverse have been set,
# it´s returned from the chache (in the parent environment) without needing to compute it again.
# setter and getter functions for both the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        # Initializing variable for the inverse of the matrix
        i <- NULL
        
        #@set inner function to set the value of the matrix in the parent environment
        set <- function(y) {
                x <<- y
                # @TODO: possible improvement: If y is the same matrix as x,
                # @TODO: we do not need to nullify i 
                i <<- NULL
        }
        
        #@get inner function to get the cached value of the matrix
        get <- function() x
        
        #@setinv inner function to set the inverse of the matrix
        setinv <- function(inv) i <<- inv
        
        #@getinv inner function to get the cached inverse of the matrix
        getinv <- function() i
        
        # Value returned. A named list with the four getter/setter functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
        
                

}


## Function that, from a parameter of the type returned by  makeCacheMatrix
## returns the inverse of a matrix. If the inverse was already cached, this function returns this value.
## otherwise, it previously computes the inverse and sets it in the makeCacheMatrix,

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
        i <- x$getinv()
        # Checks if inverse is already in x
        if(!is.null(i)) {
                message("getting cached data")
                # @TODO: possible improvement: There should be only one exit point from a function
                # @TODO: delete this 'return' and use an 'else' for inverse computation
                return(i)
        }
        data <- x$get()
        print("calculando")
        i <- solve(data, ...)
        x$setinv(i)
        i
        
        
}
