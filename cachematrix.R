## The intention is to do the Programming Assignment2
## functions do use of the cache to store results and minor time-consuming
## computations that can be repeated

## makeCacheMatrix cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinvmat <- function(solve) m <<- solve
        getinvmat <- function() m
        list(set=set, get=get,
             setinvmat = setinvmat,
             getinvmat = getinvmat)
}


## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinvmat()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvmat(m)
        m
}
