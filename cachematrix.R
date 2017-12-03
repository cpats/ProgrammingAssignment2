## Contains two functions: makeCacheMatrix and cacheSolve

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inverseMatrix <<- solve
  getInverse <- function() inverseMatrix
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix
}
