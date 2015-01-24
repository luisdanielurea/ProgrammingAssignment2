## Solve the inverse of a matrix inside a matrix cache object. 
## Check inside the matrix cache object if it was already, in case it was get the result already calculated 
## if not, solve it and store the inverse.

## A cache object that store a matrix and its inverse after it was calculated

makeCacheMatrix <- function(x = matrix()) {
     Inv <- NULL
     set <- function(y) {
         x <<- y
         Inv <<- NULL
     }
     get <- function() x
     setInv <- function(solve) Inv <<- solve
     getInv <- function() Inv
     list(set = set, get = get,
          setInv = setInv,
          getInv = getInv)
 }


## Invoke makeCacheMatrix in order to know if the Inverse was calculated
## If it was calucaled take from the cache matrix object, if not, it calculates it and store it inside it.


cacheSolve <- function(x,...) {
     Inv <- x$getInv()
     if(!is.null(Inv)) {
         message("Inverse already calculated")
         return(Inv)
     }
     mat <- x$get()
     Inv <- solve(mat,...)
     x$setInv(Inv)
     Inv
 }
