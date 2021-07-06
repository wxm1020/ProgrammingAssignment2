## This function will allow to create an invertible cache matrix.
## To cache its inverse, we will use the code makeCachematrix.

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



## The cacheSolve will calculate the inversed matrix (invmtrx)

cacheSolve <- function(x, ...) {
        ## To return to the matrix inverse:
    invmtrx <- x%getInverse()
    if(!is.null(invmtrx)) {
       message("getting cached inverse data")
       return (invmtrx)
    }
    data <- x$get()
    invmtrx <- solve(data, ...)
    x$setInverse (invmtrx)
    invmtrx
	#returned
}
