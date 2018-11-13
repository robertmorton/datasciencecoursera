## This is taking in a matrix and creating a cached
## version of it.

makeCacheMatrix <- function(x = matrix()) {
        
    athing <- NULL
        
    set <- function(y) {
        x <<- y
        athing <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) athing <<- inverse
    getinverse <- function() athing
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## If the solve function is working like I think it
## it does this should solve a cached version of
## the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    athing <- solve(x)
    if(!is.null(athing)) {
          message("getting cached data")
          return(athing)
    }
    data <- x$get()
    athing <- inverse(data,  ...)
    x$setinverse(athing)
    athing
}

