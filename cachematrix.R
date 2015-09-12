## the function makeCacheMatrix() creates functions to set
## a matrix, get a matrix, set an inverse of a matrix, and get the inverse of a matrix.
## The function cacheSolve() returns the inverse of a matrix




makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  ## defines the function for setting x to some value, y, while setting inverse to NULL  
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  ## defines functions for getting the value of x, setting the value of the inverse, i, and
  ##getting this value, i
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  ##returns the list of functions defined above
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of the given matrix, either through caclculation,
## or by retrieving the value from the cache

cacheSolve <- function(x, ...) {
  ##Calls the getinverse function to see whether the value of i has been cached.
  ##If so, it returns the cached value
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  ##Gets the value of x, and calculates the inverse  
  data <- x$get()
  i <- solve(x)
  x$setinverse(i)
  i
}