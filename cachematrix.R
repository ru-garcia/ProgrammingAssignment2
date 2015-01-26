## The following two functions compute the inverse of a matrix and cache
## the inverse for easy retrieval without having to repeatedly compute it,
## if the matrix hasn't changed. It assumed that the matrix supplied is
## always invertible.


## The first function below (makeCacheMatrix) creates a special matrix
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinvese = getinverse)
}


## The second function (cacheSolve) computes the inverse of the special
## matrix returned by the above function (makeCacheMatrix). If the inverse
## has already been calculated and the matrix has not changed, then
## cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
                m <- x$getinvese()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m <- solve(data, ...)
                x$setinverse(m)
                m
}
