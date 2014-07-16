## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL # cached inverse of matrix
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        # get function
        get <- function() {x}

        # set inv
        setinv <- function(inverse) {inv <<- inverse}

        # get inv
        getinv <- function() {inv}

        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return (inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
