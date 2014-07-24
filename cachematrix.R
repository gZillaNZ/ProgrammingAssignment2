## Contains 2 functions 
## (1) makeCacheMatrix with getter and setter for
## a matrix and it's inverse
## (2) cacheSolve to return either a previously saved
## matrix inverse or to calculate a new inverse if not found
## in the cache or matrix has changed

## (1)
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## set the passed matrix in cache
  set <- function(y) {
    x <<- y
    m<<- NULL
  }
  
  ## return the cached matrix
  get <- function() x
  
  ## set the inverse of the matrix in cache
  setinv <- function(inv) m <<- inv
  
  ## return the cached inverse
  getinv <- function() m
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## (2)
cacheSolve <- function(x, ...) {
  ## try to get the inverse value from cache, if found use cached value
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## if cached inverse not found get matrix, calculate inverse and set 
  ## in cache, finally return inverse
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m

}
