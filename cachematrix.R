## makeCacheMatrix create a special matirx to restore the value of the inverse of x
## and cacheSolve get the data from makeCacheMatrix and see if it has it.if 
##it has it, then it will jsut use the cache, if it doesnt, it will cacluate the inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x                                 ##signing x to get
  setInverse <- function(Inverse) m <<- Inverse       ##creating the list of functions
  getInverse<- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...){
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()                         
  if(!is.null(m)) {                                  ##checking if the cache has the value.
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)                             ## solve the data when it doesnt contain the cache value
  x$setInverse(m)
  m
}