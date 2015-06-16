

##There are two functions makeCacheMatrix() and cacheSolve() to calculate the inverse of a matrix in a more efficient way by cacheing the inverse of the matrix.



## makeCacheMatrix() function takes in a matrix and provides four functions that caches and returns both the matrix and the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 set <- function(y){
   x <<- y
   inv <<- NULL
 }
 get <- function() x
 setinv <- function(solved) inv <<- solved
 getinv <- function() inv
 list(set = set, get = get, setinv = setinv, getinv = getinv)
}




## cacheSolve() function uses the cached matrix from makeCachedMatrix() function and checks if there is an inverse of that matrix already calculated. 
## If the inverse matrix is already cached, its returns the cached inverse. Otherwise, it calculates the inverse and caches it for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
      message("Getting cached inverse")
      return(inv)
  }
  data = x$get()
  solved <- solve(data,...)
  x$setinv(solved)
  solved
}
