## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function


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
