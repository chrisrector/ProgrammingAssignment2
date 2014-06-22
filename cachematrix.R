## These functions create a special matrix object that can 
## cache it's inverse using the cacheSolve function
## functions do

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## This function returns the inverse of a matrix but will check if
## the inverse has already been cached before calculating it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  message("calculating inverse")
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
