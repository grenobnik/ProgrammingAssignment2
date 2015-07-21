###
# The following two functions can be used to cache an expensive operation
# of inverting a matrix. The first function 'makeCacheMatrix' creates
# a "special object" that can be later used by the second function
# 'cacheSolve' in order to either compute the inverse if it has not been
# computed yet, or just return a cached inverse matrix without recomputing it.

###
# Create a "special object" (a list in reality) that performs four operations:
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse matrix
# 4. get the inverse matrix
# This migh be used to cache the inverse of a matrix,
# this "special object" to be used by 'cacheSolve' function.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL # local storage for cached value of the inverse matrix
  set <- function(y) {
    x <<- y # local storage for matrix itself
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(i) {
    inv <<- i
  }
  getInverse <- function() {
    inv
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


###
# The argument 'x' should be created by 'makeCacheMatrix' function,
# compute the inverse of the matrix in 'x' object if it has not been done
# and cache its value back in 'x' object; just return the cached value
# of the inverse if it is known already.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse() # get the inverse from the "special object"
  if(!is.null(inv)) {   # if the inverse is not NULL, just return it
    message("getting cached data")
    return(inv)
  }                     # if the inverse is NULL, compute it:
  m <- x$get()          # - get the matrix
  inv <- solve(m, ...)  # - compute the inverse
  x$setInverse(inv)     # - store the inverse in 'x'
  inv                   # - return the inverse
}
