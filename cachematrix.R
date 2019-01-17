## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #initialize the inv matrix as NULL
  set <- function(y) {
      x <<- y
      inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse #set the inverse as setInverse
  getInv <- function() inv
  list(set = set, get=get, setInverse = setInverse, getInv = getInv)
}


## This function solves for the inverse of a matrix. If the inverse has already been calculated, it pulls the cached inverse opposed to recalculating.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } #if statement looks to see if the inverse already exists and returns the cached inverse if it does
  data <- x$get()
    m <- solve(data) #if the inverse doesn't already exist, this part calculates it
    x$setInv(m)
    m
}