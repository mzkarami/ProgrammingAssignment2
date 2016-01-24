## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object 
## that can cache its inverse.
## Keep in mind that the matrix being passed needs to be non-singular,
## otherwise is make no sense to call solve on as singular matrix
makeCacheMatrix <- function(x = matrix()) {

  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinversematrix <- function(solve) s <<- solve
  getinversematrix <- function() s
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## Write a short comment describing this function
## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinversematrix();
  if(!is.null(s)) {
    message("getting the inverse matrix")
    return(s)
  }
  data <- x$get()
  
  s <- solve(t(data) %*% data, ...)
  x$setinversematrix(s)
  s
}
