## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix first declares a null inverse. 
## It then calculates the inverse of the matrix,
## setting appropriate calls such as setInv and
## getInv.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
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


## cacheSolve checks to see if a matrix has already
## been calculated. If so, it returns the cached data.
## If not, it then calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
