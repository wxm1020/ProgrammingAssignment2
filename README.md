##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

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

##Please include your own comment to explain your code (Required in Rubric)

cacheSolve <- function(x, ...) {
## 
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

week3 peer assignment 2
r programming
lexical scoping
jsdf sksfsklfjsd safhsjkfhjafh afjklafakfjaf  afkakfaj alkf afj akljfajfal fjakf jakfjaoaqfjWOQJWFI ALKDJA FIAE FALKFJ AEFQAF OIAJ FOIUF AFA OFUQIU FQAOIFJA IU8UF AKFJA IU QKFJ 
viva mapua
haha
sana kaya pa natin lezzgo guys
