## This module is composed of two functions, makeCacheMatrix and cacheSolve.
## Working in conjunction, a cached matrix is created and then if an inverse
## of that matrix has not been cached, it is created using the solve function
## and then stored in the cached-matrix cache. The inverse of the matrix can
## then be retrieved from the cache.


## makeCacheMatrix creates an empty cached matrix with data attribute, inv,
## and method attributes, set, get, set_inv, and get_inv. inv is the inverse
## of the matrix. set loads matrix data into the cached matrix. get retrieves
## the matrix data. set_inv loads data of the inverse of the martrix. get_inv
## retrieves the inverse of the matrix data. This method utilizes the '<<-'
## operator which allows for assignment of values to objects which exist in an
## environment other than the current environment. This function requires that
## the matrix be square and throws an exception otherwise.

makeCacheMatrix <- function(x = matrix()) {
    if (nrow(x) != ncol(x)) stop ('matrix must be square')
    inv <- NULL
    set <- function(y) {
        if (nrow(y) != ncol(y)) stop ('matrix must be square')
        x <<- y
        inv <<- NULL
    }

    get <- function() x
    set_inv <- function(solve) inv <<- solve
    get_inv <- function() inv
    list(set = set, get = get,
         set_inv = set_inv,
         get_inv = get_inv)
}


## cacheSolve retrieves the inverse of the matrix. If the inverse exists
## in the cache, a message telling the user that the inverse is being
## retrieved from the cache and the inverse matrix is returned. If it does
## not, the cached matrix is retrieved and its inverse matrix is computed
## using the solve function. The inverse matrix is loaded into the cache and
## then the inverse matrix is returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get_inv()
        if(!is.null(inv)) {
            message('getting cached inverse')
            return(inv)
        }

        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$set_inv(inv)
        inv
}
