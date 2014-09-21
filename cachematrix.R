## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize storage for inverse
  m <- NULL
  ## Set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  ## Get the matrix
  get <- function() x
  
  ## Set the inverse of the matrix
  setInverse <- function(inverse) m <<-inverse
  ## Get the inverse of the matrix
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    ## Return the inverse if already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  ## Get the matrix from our object
  data <- x$get()
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  ## Set the inverse to the object
  x$setInverse(m)
  ## Return the matrix
  m

}
