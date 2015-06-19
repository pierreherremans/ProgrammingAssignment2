


## This function makes a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function()
                x
        setinverse <- function(inv)
                inverse <<- inv
        getinverse <- function()
                inverse
        list(
                set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getinverse()
        if (!is.null(inverseMatrix)) {
                message("getting cached data ")
                return(inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setinverse(inverseMatrix)
        inverseMatrix
}
