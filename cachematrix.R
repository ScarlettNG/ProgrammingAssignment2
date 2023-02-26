## We will write a function that can cache the 
## inverse of the created matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Initialise the matrix
  i <- NULL
  
  ## Set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Get the matrix
  get <- function() x
  
  ## Set the inverse
  set_inv <- function(inverse) {
    i <<- inverse
  }
  
  ## Get the inverse
  get_inv <- function() {
    i
  }
  # return the list of all funtions
  list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}

## Let's compute the matrix got above. If the inverse
## been calculated, the 'cacheSolve' function will return 
## the inverse from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$get_inv()
  
  ## Return the inverse if it's been calculated already
  if (!is.null(i)) {
    message('getting cached data')
    return(i)
  }
  
  ## Get the matrix
  data <- x$get()
  
  ## Calculate the inverse
  i <- solve(data,...)
  x$set_inv(i)
  
  ## return the matrix 
  i
}
