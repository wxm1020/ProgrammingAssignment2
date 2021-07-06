##In this example the <<- operator may be used for the purpose of assigning a value to an object in a different environment. Below are two functions used to generate a custom object which contains a numerical vector and its mean cache.

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
  x <<- y
  j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}

##In this example the <<- operator can be used to assign is introduced. In the aforementioned procedure, the following function calculates the mean of the particular vector produced. It checks, however, first to see whether the average was calculated. If so, the mean is obtained from the cache and the computer is skipped. If not, the mean of the data is calculated and the average value in a cache is determined via the defined function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  j <- x$getInverse()
  if(!is.null(j)){
  message("getting cached data")
  return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}