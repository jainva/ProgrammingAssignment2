## Below are two functions, 
##	first function can be used to cahe a matrix
##	second function utilize first function to check if it already contain inverse of the matrix, otherwise it calculate inverse and store in first function for future calculations

## Below function create a special matrix, which is actually list of functions to set/retrieve original matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

	  invrs <- NULL
        set <- function(y) {
                x <<- y
                invrs <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) invrs <<- inverse
        getinverse <- function() invrs
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function unitilize the first function to see if already contain inverse of first function. If the inverse is not available then it calcualte inverse and store in the 
## first function for future computations.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

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
