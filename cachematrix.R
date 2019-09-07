## Both functions together allow to solve a matrix.
## In the case the matrix has already been solved before, the second function
## returns the solution from the cache instead of computing it again.

## makeCacheMatrix() returns a list of 4 functions :
## set() pushes the matrix to the cache
## get() pulls the matrix from the cache
## setinverse() pushes the matrix solved by the solve() function of cachesolve()
## getinverse() pulls a previously solved matrix from the cache
## none of these functions are called during the execution of makeCacheMatrix(); they are only defined

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

## cacheSolve() uses the getinverse() function in order to evaluate
## if a matrix contained in an object returned by makeCacheMatrix()
## (let's say myCachedMatrix) has already been solved. Two possible scenarios:
## 1) If the matrix has not been solved yet (i.e. getinverse() returns NULL), it does the calculation
## and also pushes the solved matrix to the myCachedMatrix via the setinverse() function.
## 2) If the matrix has already been solved (i.e. getinverse() does not return NULL), it uses the
## getinverse() function to pull the value stored in the myCachedMatrix

cacheSolve <- function(x, ...) {
  if(is.null(x$getinverse())) {
    x$setinverse(solve(x$get(), ...))
    return(x$getinverse())
  } else {
    message("getting cached data")
    return(x$getinverse())
  }
}
