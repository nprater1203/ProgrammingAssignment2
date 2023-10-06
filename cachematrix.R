# This function leverages the makeVector function written in the assingment
# description, but is modified to create a special "matrix" 
# object that can cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setSolve <- function(solve) m <<- solve
  getSolve <- function() m
  
  list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


# This function leverages the cachemean function in the description
# of the assignment. It finds the inverse of the special "matrix" that was
# retunred from the makeCahceMatrix. If the matrix has already been solved
# the it should just get that value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getSolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setSolve(m)
  m
}
