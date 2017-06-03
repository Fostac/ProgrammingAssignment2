## these pair of functions let you convert an invertible matrix to a special 
## matrix that is cached after being calculated once. Afterwards the result
## will be taken from the cache instead of beeing calculated a second time.
## Usage:
## myMat<- ##define invertible matrix
## myCachedMat<-makeCacheMatrix(myMat)
## cacheSolve(myCachedMat)

## function makeCacheMAtrix takes an invertible matrix and converts it to a special
## matrix for the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## function cacheSolve gives the inverse of a matrix as an output. if the matrix
## has been calculated before the result is taken from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
  ## Return a matrix that is the inverse of 'x'
}