## These two functions enable a user (1) to create a special "matrix" 
## object, where the matrix inverse can be cached, and (2) to return 
## the inverse by looking it up in the cache, or if nothing is stored 
## in the cache, to calculate the inverse and store it in the cache.

## This function creates a special "matrix" object that allows you to cache
## the inverse of a matrix. This function outputs a list of four functions
## that allows you to set and get the matrix and set and get the inverse.

makeCacheMatrix <- function(x = matrix()) {
     i <- NULL
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) i <<- inverse
     getinverse <- function() i
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function will look up the inverse for the special "matrix" object
## in the cache. If there is no inverse stored in the cache, it will then
## compute its inverse and store that in the cache.

cacheSolve <- function(x, ...) {
     i <- x$getinverse()
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinverse(i)
     i
}
