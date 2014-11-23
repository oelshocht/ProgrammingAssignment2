## The following pair of functions cache the inverse of a matrix. Inversing
## a matrix is a computationnly costly operation so there may be some benefit
## in caching its result.
## The first function constructs and returns a special matrix object.
## The second function computes the inverse of such a matrix object, or returns
## the cached reslt if it has been computed before.

## Create and return a special matrix object.
## The object contains
##     - x (function parameter): the matrix itself
##     - i (local variable, part of the environment): the cached result
##       of the matrix inversion operation (or NULL if it has not been computed)
## as well as functions to set and get the 2 valued above

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        ## x and i are free variables here.
        ## Unlike the <- operator which creates new local variables,
        ## the <<- operator searches for a definition in the parent environment
        x <<- y
        i <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function (inverse) {
        ## Save a cached copy of the inverse operation in the variable
        ## defined in the parent environment
        i <<- inverse
    }
    getinverse <- function() {
        i
    }
    list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Return the cached copy of the matrix inversion operation.

cacheSolve <- function(x, ...) {
    ## Retrieve the cached result
    i <- x$getinverse()

    ## NULL indicates that the matrix inverse has not been computed before; in that
    ## case, compute the result and save it back into the special matrix object.
    if (is.null(i)) {
        i = solve(x$get())
        x$setinverse(i)
    }
    i
}
