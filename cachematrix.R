## This function is aided to speed up a (hypothetical) process that involves using several times the inverse of a matrix
## Calculating the inverse of a matrix is, in general, a constly computation

## This function allows to "cache" the inverse of a matriz creating a sort of matrix 

makeCacheMatrix <- function(x = numeric()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInverse <- function(solve) m <<- solve
     getInverse <- function() m
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## The following function computes the inverse of the object (this sort ofmatrix) returned by the funtcion makeCacheMatrix created above. 
## If the matrix has not changed and its inverse has already been calculated, then the cachesolve detects this fact and shaves 
## computer resources by retrieving the inverse from the cache

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getInverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setInverse(m)
     m
}


