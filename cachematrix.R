## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #inverse is initially NULL
  i <- NULL
  
  #matrix setter
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  #matrix getter
  get <- function() x
  
  #inverse setter
  setInverse <- function(inv) i <<- inv
  
  #inverse getter
  getInverse <- function() i
  
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
  #get inverse and check if it was already calculated and cached
  #if it is return cached value
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  
  #calculate and cache inverse if cache was previously empty(NULL)
  m = x$get()
  i <- solve(m, ...)
  x$setInverse(i)
  i
}
