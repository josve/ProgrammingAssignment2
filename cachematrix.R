## These functions provide functionality to create matrices that support
## cached computations of matrix inversion. This is to efficiently cache
## that computation without needing to take care of it in every place it
## is used.

makeCacheMatrix <- function(x = matrix()) {
    # Creates a matrix that can cache the inverse of itself.
    #
    # Args:
    #   x: The matrix that should be cached, if no matrix is provided
    #      then an empty matrix is used.
    #
    # Returns:
    #   A matrix that caches inverse of itself when it is computed.
    
    # Check if the input is a matrix, otherwise warn and coerce into a matrix.
    if (!is.matrix(x)) {
        warning("x is is not a matrix.")
        x <- as.matrix(x)
    }
    
    # The cached inversed matrix, initially null.
    inverseMatrix <- NULL
    
    set <- function (y) {
        # Change the value of the matrix, since we change the matrix we
        # also need to clear the cached inverseMatrix.
        x <<- y
        inverseMatrix <<- NULL
    }
    
    get <- function() {
        # Just get the current matrix value.
        x
    }
    
    # Used by the caching inverse matrix function to store the cached
    # matrix.
    setInverseMatrix <- function (i) {
        inverseMatrix <<- i
    }
    
    # Used by the caching inverse matrix function to get (if not NULL) the
    # cached matrix.
    getInverseMatrix <- function() {
        inverseMatrix
    }
    
    list (set = set, 
          get = get, 
          setInverseMatrix = setInverseMatrix, 
          getInverseMatrix = getInverseMatrix)
}

cacheSolve <- function(x, ...) {
    # Returns the inverse a matrix, caching the value for future calls.
    #
    # Args:
    #   x: The matrix that should be inverted, it is assumed to be square.
    #   ...: The additional arguments will be sent to the solve call.
    #
    # Returns:
    #   The inverse of the specificed matrix.
    
    # Validate that we have a cachable matrix as input, we do that by
    # validating that the specified names exists in the input object.
    xNames <- names(x)    
    if (!("get" %in% xNames &&
          "setInverseMatrix" %in% xNames &&
           "getInverseMatrix" %in% xNames)) {
        stop("Not a cachable matrix.")
    }
    
    # First check the cached value.
    inverse <- x$getInverseMatrix()
    if (!is.null(inverse)) {
        message("getting cached data")
        
        # We can now just return the old cached value.
        return (inverse)
    }
    
    # Get the matrix to use for the calculation
    matrix <- x$get()

    # This is actually a bit dubious since the cache key doesn't include
    # any additional parameters so if you send different arguments you will
    # still get a cache hit from the first one. But I'm assuming that this is
    # what's wanted.
    inverse <- solve(matrix, ...)
    
    # Cache the value.
    x$setInverseMatrix(inverse)
    
    # Return the computed inverse matrix.
    inverse
}
