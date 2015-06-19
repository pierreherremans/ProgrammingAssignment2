## This pair of functions  implements matrix inversion with caching.
## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than
## compute it repeatedly.
## Usage example (inspired by the solve {base} R Documentation):
## hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
## h8 <- hilbert(8);
## m <- makeCacheMatrix(h8)
## s <- cacheSolve(m); s
## s <- cacheSolve(m); s


## This function makes a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(inv) {
                inverse <<- inv
        }
        getinverse <- function() {
                inverse
        }
        
        list(
                set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse
        )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getinverse()
        if (!is.null(inverseMatrix)) {
                message("getting cached data ")
                return(inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setinverse(inverseMatrix)
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix
}
