##In this example the <<- operator may be used for the purpose of assigning a value to an object in a different environment. Below are two functions used to generate a custom object which contains a numerical vector and its mean cache.
The primary way to interact with me and the other students in this course is through the discussion forums. Here, you can start new threads by asking questions or you can respond to other people's questions. If you have a question about any aspect of the course, I strongly suggest that you search through the discussion boards first to see if anyone has already asked that question. If you see something similar to what you want to ask, you should up-vote that question using the up-arrow button rather than asking your question separately. The more votes a question or comment gets, the more likely it is that I will see it and be able to respond quickly. Of course, if you don't see a question similar to the one you want to ask, then you should definitely start a new thread on the appropriate forum.

makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

##In this example the <<- operator can be used to assign is introduced. In the aforementioned procedure, the following function calculates the mean of the particular vector produced. It checks, however, first to see whether the average was calculated. If so, the mean is obtained from the cache and the computer is skipped. If not, the mean of the data is calculated and the average value in a cache is determined via the defined function.

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting inversed matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}

##Put comments here that give an overall description of what your functions do Our aim in this experiment is to write a pair of functions, namely, "makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix Write a short comment describing this function makeCacheMatrix is a function which creates a special "matrix" object that can cache its inverse for the input (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {

inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
}
get <- function() x
setinv <- function(inverse) inv <<- inverse
getinv <- function() inv
list(set = set, get = get, setinv = setinv, getinv = getinv)
}

##Write a short comment describing this function cacheSolve is a function which computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
inv <- x$getinv()
if(!is.null(inv)) {
message("getting cached result")
return(inv)
}
data <- x$get()
inv <- solve(data, ...)
x$setinv(inv)
inv
}

## Caching the Inverse of a Matrix: Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly. Below are a pair of functions that are used to create a special object that stores a matrix and caches its inverse. This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
}
get <- function() x
setInverse <- function(inverse) inv <<- inverse
getInverse <- function() inv
list(set = set,
get = get,
setInverse = setInverse,
getInverse = getInverse)
}

