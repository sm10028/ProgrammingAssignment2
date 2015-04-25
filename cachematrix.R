
##
## A pair of functions to cache the production of the inverse of a matrix. 
##
## Based on example code for cached production of the mean of a list within programming assignment.
##
## usage example:
## 
## "a<-makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(a)
## cacheSolve(a)"
##
## where the second call to cacheSolve produces the cached inverse matrix
##
##
## MakeCacheMatrix creates a list containing four functions to get and set the value of the matrix and  
## its inverse. The get functions simply return the values of the matix and its inverse. 
## The set functions use the <<- operator to update the values in the enclosing enviroment (the list)

makeCacheMatrix <- function(x = matrix()) {
inv<- NULL
set <- function(y) {
        x <<- y
        inv <<- NULL
}
get <- function() x
setinv <- function(z) inv <<- z
getinv <- function() inv
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
        
}


## tests whether inv is defined in the list, if so it returns it, if not it calculates it, stores it, and returns in

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinv()
        
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        data <- x$get()
        inv<- solve(data, ...)
        x$setinv(inv)
        inv

}

