## Put comments here that give an overall description of what your
## functions do
##      makeCacheMatrix: This function creates a special "matrix" object
##              that can cache its inverse.
##      cacheSolve: This function computes the inverse of the special
##              "matrix" returned by makeCacheMatrix above. If the inverse
##              has already been calculated (and the matrix has not changed),
##              then cacheSolve should retrieve the inverse from the cache.
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

## computing the inverse of a square matrix can be done with the solve function
## in R. For example, if X is a square invertible matrix, then solve(X)
## returns its inverse.

makeCacheMatrix <- function(x = matrix()) {

        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(x) m <<- x
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function
## The following function creates an inverse of the special "matrix"
## with the above function. However, it first checks to see if the matrix has
## already been inversed. If so, it gets the inverse from the cache and
## skips the inverse operation. Otherwise, it creates the inverse of the matrix
## and sets the value in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)

        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}
