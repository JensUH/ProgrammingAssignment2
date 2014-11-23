## These two functions cahces the inverse of matrix.


## The function "makeCacheMatrix" create a special matrix
## object that can cache the inverse of the matrix. It 
## allows for the self explanatory methods "get", "set",
## "setinverse", and "getinverse".

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(mat) inv <<- mat
     getinverse <- function() inv
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## The function "cacheSolve" calculate the inverse of matrix
## of the object type created by "makeCacheMatrix". If the
## inverse is already cached, the function just gets it,
## ohterwise it calculate it again.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     invs <- x$getinverse()
     if(!is.null(invs)) {
          message("getting cached data")
          return(invs)
     }
     mat <- x$get()
     invs <- solve(mat, ...)
     x$setinverse(invs)
     invs
}
