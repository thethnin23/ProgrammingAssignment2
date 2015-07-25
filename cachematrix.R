

##There are two functions makeCacheMatrix() and cacheSolve() to calculate the inverse of a matrix in a more efficient way by cacheing the inverse of the matrix.



## makeCacheMatrix() function takes in a matrix and provides four functions that caches and returns both the matrix and the inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {

 inv <- NULL
 ## cache the matrix to be inversed
 set <- function(y){
   x <<- y
   inv <<- NULL
 }
 
 ##get the cached matrix
 get <- function() x
 ## cache the inverse matrix 
 setinv <- function(solved) inv <<- solved
 ## get the cached inverse matrix
 getinv <- function() inv
 list(set = set, get = get, setinv = setinv, getinv = getinv)
}




## cacheSolve() function uses the cached matrix from makeCachedMatrix() function and checks if there is an inverse of that matrix already calculated. 
## If the inverse matrix is already cached, its returns the cached inverse. Otherwise, it calculates the inverse and caches it for future use.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
		
## check if inverse of the matrix is cached
  inv <- x$getinv()  
  if(!is.null(inv)){
      message("Getting cached inverse")
      return(inv)
  }
  ## if the inverse is not cached yet, get the cached matrix
  data = x$get()
  ## solve for the inverse
  solved <- solve(data,...)
  ## cached the solved inverse for future use
  x$setinv(solved)
  ## return the solved inverse
  solved
}
