## These functions will calculate the inverse of a square matrix
## and cache the result for further use.

## makeCacheMatrix returns a list of 4 functions - these are described below.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL             #inv is resetted everytime makeCacheMatrix is called.  
  set <- function(y) {    #This function changes the square matrix and
    x <<- y               #resets the inv
    inv <<- NULL
  }
  get <- function() x     #This function displays the matrix
  setinv <- function(solve) inv <<- solve #This function is called via cacheSolve and obtains and stores the inverse of the matrix.
  getinv <- function() inv                #This function displays the inverse of the matrix
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns the inverse of the matrix from makeCacheMatrix. If inv is null,
## inv is calculated. If inv has been stored, the value is obained from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
