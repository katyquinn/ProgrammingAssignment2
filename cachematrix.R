## This pair of functions, makeCacheMatrix and cacheSolve, caches the inverse of a matrix.
## Example of usage:
## mdat <- matrix(c(1,2,3,4,5,6,7,8,.1),nrow=3,ncol=3) # create matrix
## mdatcache <- makeCacheMatrix(mdat) # create special matrix object to
##                                    # use with cacheSolve
## minv1 <- cacheSolve(mdatcache) # compute inverse of special matrix object
## minv2 <- cacheSolve(mdatcache) # when called a 2nd time using mdatcache,
##                                # will used cached inverse

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.
## Computing the inverse of a square matrix can be done with the 
## solve function in R. For example, if X is a square invertible 
## matrix, then solve(X) returns its inverse.
## Assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
