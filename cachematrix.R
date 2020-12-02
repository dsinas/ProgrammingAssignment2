################################################################################
## 
## Functions to create a special matrix object that can cache the calculated 
## inverse of the matrix. This avoids recalculating the matrix inverse!
## 
## example usage:
##    source('cachematrix.R')
##    mat0 <- matrix(c(1, 2, 3, 4), 2, 2)	# create a matrix
##	  smat <- makeCacheMatrix(mat0)	    	# create a special matrix object
##	  cacheSolve(smat)	              		# calculate inverse of a matrix
##	  cacheSolve(smat)	              		# retrieve the calculated inverse
##
################################################################################
################################################################################
## 
## The 'makeCacheMatrix' function gets a matrix as input and creates an object 
## with four methods to 'get' and 'set' the input matrix and also to calculate 
## the inverse of the matrix and cache it.
## 
################################################################################
makeCacheMatrix <- function(x = matrix()) {
  
  minv <- NULL
  
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(solve) minv <<- solve
  
  getinv <- function() minv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

################################################################################
## 
## The 'cacheSolve' function gets a special matrix created by 'makeCacheMatrix'
## and returns its inverse. When the matrix is called for the first time the 
## function calculates and stores the inverse, otherwise the function retrieves 
## the previously calculated inverse.
## 
################################################################################
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minv <- x$getinv()
  
  if(!is.null(minv)) {
    message('getting cached data')
    return(minv)
  }
  
  data <- x$get()
  
  minv <- solve(data, ...)
  
  x$setinv(minv)
  
  minv
  
}
