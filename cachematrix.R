## makeCacheMatrix creates a list of functions that, upon the input of a matrix
## x, gets or sets the value of the matrix and then gets or solves for the 
## inverse of that matrix. cacheSolve is then used to check if the matrix has
## already been solved, and if it has then it gets the inverse from the cache,
## otherwise it solves for and then saves to the cache the inverse of the matrix

## makeCacheMatrix creates a list of functions that set or get the value of a 
## matrix and to set or get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 m  <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function checks a matrix to see if the inverse of the matrix has already 
## been solved for, and if so returns it, otherwise it solves and sets for the
## inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
