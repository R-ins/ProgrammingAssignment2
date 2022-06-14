## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <- function(y) {
    x <<- y
    inve <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inve <<- solve
  getInverse <- function() inve
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ive <- x$getInverse()
  if (!is.null(inve)) {
    message("getting cached data")
    return(inve)
  }
  date <- x$get()
  inve <- solve(date, ...)
  x$setInverse(inve)
  inve
}

