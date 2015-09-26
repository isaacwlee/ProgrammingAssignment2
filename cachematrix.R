# The functions below creates a matrix object that can cache its inverse.

# The first function, makeCacheMatrix creates a special "matrix". It performs following 4 steps...
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of the inverse matrix
# 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y) {
      x <<- y
      invrs <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) invrs <<- solve
    getinverse <- function() invrs
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
        )
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invrs <- x$getinverse()
    if(!is.null(invrs)) {
      message("getting cached data")
      return(invrs)
    }
    data <- x$get()
    invrs <- solve(data)
    x$setinverse(invrs)
    invrs
}
