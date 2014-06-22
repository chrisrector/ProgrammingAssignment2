## These functions create a special matrix object that can 
## cache it's inverse using the cacheSolve function


## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  # here we set the inv value in the parent environment:
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## This function returns the inverse of a matrix but will check if
## the inverse has already been cached before calculating it

cacheSolve <- function(x, ...) {
  
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  # if here we did not find a cached inverse value
  message("calculating inverse")
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
