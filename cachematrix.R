## Functions to evaluate inverted matrices with caching ability

## This function creates a cacheable matrix which can be solved in cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
  
  # Set inverted matrix as NULL
  inv.matrix <- NULL
  
  set <- function(y) {
    x <<- y
    inv.matrix <<- NULL
  }
  get <- function() x
  
  setinv <- function(solve) inv.matrix <<- solve
  getinv <- function() inv.matrix

  list(
    set = set,
    get = get,
    setinv = setinv,
    getinv = getinv)
}


## Evaluates the inverse of a matrix defined in makeCacheMatrix() 
## Returns the cached inverse if this has already been evaluated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # Check for cached inverse matrix
  m <- x$getinv()
  if(!is.null(m)){
    message("Retriving cached inverse matrix")
    return(m)
  }
  
  # Calculate inverted matrix using solve()
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  
  # Return inverted matrix
  m
}
