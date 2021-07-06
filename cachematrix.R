## Usually matrix inversion is a costly process and it might help to reverse a matrix instead of continually computing it (there are also alternatives to matrix inversion that we will not discuss here). Your task is to develop a few of functions that cache the opposite of a matrix.
makeCacheMatrix <- function(x = matrix()) {
    invmtrx<- NULL
    set<- function(y){
	    x <<- y
            invmtrx <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) invmtrx <<- inverse
    getInverse<-function() invmtrx 
    list(set= set, get= get, 
	 setInverse = setInverse, getInverse= getInverse)
    }



## The solution function in R can be used to calculate the inverse of a square matrix. For example, when X is an invertible square matrix, solve(X).

cacheSolve <- function(x, ...) {
        
    invmtrx <- x%getInverse()
    if(!is.null(invmtrx)) {
       message("getting cached inverse data")
       return (invmtrx)
    }
    data <- x$get()
    invmtrx <- solve(data, ...)
    x$setInverse (invmtrx)
    invmtrx
	
}
