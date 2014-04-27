# The file provides an improved R 'matrix' that caches the matrix inverse for speed improvement
# To make a cache matrix, pass a regular R matrix to makeCacheMatrix
# To access the inverse, call cacheSolve, which (re)computes the inverse as necessary

# pass this function a matrix
# Inverse is computed only when cacheSolve (see below)
# is called for the first time after set-ting the data
makeCacheMatrix <- function(x = matrix()) {
  mInv <- NULL
  
  # get methods
  get <- function() x
  getInv <- function() mInv # cached value returned
  
  # update the matrix
  # resets the cached inverse
  set <- function(y) { 
    x <<- y
    mInv <<- NULL
  }  
  # update the cached inverse
  setInv <- function(inv) mInv <<- inv
  
  list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)

}

# Solve for matrix inverse, using cached values if they exist
# and matrix data aren't updated.
# Matrix is assumed to be invertible
cacheSolve <- function(x, ...) {
  # get cached inverse, return it if non-null
  inv <- x$getInv()  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  # compute and cache inverse, then return it
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
