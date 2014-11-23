## Put comments here that give an overall description of what your
## functions do

## this function takes a matrix as an argument and stores it with an initial null matrix
## along with creating a list of 4 functions to perform on the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## this functions gets the inverse of the matrix x.  This is matrix m, which initially is set
## to null. Thus on the first calling of chacheSolve() it passes the if statement and uses 
## the code below that determine the mean and assign the inverse of x to the inverted matrix.
## on every subsequent calling of cacheSolve() the inverse of x is not null so it goes
## through the if statment and retrieves the inverted matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
