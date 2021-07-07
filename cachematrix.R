## makeCacheMatrix generates a special object "matrix," which can cache its inverse
## If the inverse is calculated (and the matrix has not changed), the inverse from the cache must be retrieved by the catchesolve

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set<- function(y){
	    x <<- y
            i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse<-function() i 
    list(set = set, get = get, 
	 setInverse = setInverse, getInverse = getInverse)
    }

## This function calculates the inverse of the special "matrix" of makeCacheMatrix
## The solution function in R can be used to calculate the reverse of a square matrix

cacheSolve <- function(x, ...) {
        
    i <- x%getInverse()
    if(!is.null(i)) {
       message("getting cached data")
       return (i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse (i)
    i
	
}
