## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a new matrix whose inverse can can be cached.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # Default value of the inverse is NULL
        # Set the matrix and inverse values to default 
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Get the input matrix
        get <- function() x
        
        # Set the inverse matrix
        set_inv <- function(inv_mat) inv <<- inv_mat
        
        # Get the inverse matrix
        get_inv <- function() inv
        
        # Return list of functions
        list(set = set, get = get, 
             set_inv = set_inv,
             get_inv = get_inv) 
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above.Takes the cached value if 
# available

cacheSolve <- function(x, ...) {
        
        # Get the inverse matrix
        inv <- x$get_inv()
        
        # Check if inverse is not NULL, and retrieve cached data
        if (!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        
        # Compute the inverse, if not cached
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inv(inv)
        inv
}
