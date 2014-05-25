## Using the example code provided, set up makeCacheMatrix and cacheSolve.
## makeCacheMatrix is set up as a matrix (x = matrix()) and replace "mean" with "solve" 
## for cacheSolve, replace all instances of "mean" with "solve" as well

makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

cacheSolve <- function(x, ...)
{
  m <- x$getsolve()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

## x <- makeCacheMatrix(matrix(c(1:4), nrow = 2, ncol=2))
## cacheSolve(x) where inverse is calculated and given 
## cacheSolve (x) where inverse is printed but with "getting cached data" this time