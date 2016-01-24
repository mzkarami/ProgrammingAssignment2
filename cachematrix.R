## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversematrix <- function(solve) m <<- solve
  getinversematrix <- function() m
  list(set = set, get = get,
       setinversematrix = setinversematrix,
       getinversematrix = getinversematrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getinversematrix()
  if(!is.null(s)) {
    message("getting the inverse matrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinversematrix(s)
  s
}
