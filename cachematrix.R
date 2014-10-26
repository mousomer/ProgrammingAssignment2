## Omer Moussaffi's matrix inverse caching (mousomer@gmail.com)
## makeCacheMatrix (x) creates a cache list and assignes matrix x to it
## cacheSolve (x) gets a cache list with a martix, and outputs it's inverse. 
##    It generates a message if using the cached value.


## create a matrix cache structure for storing a matrix x
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## Output the inverse of the matrix saved in the cache structure x. 
## If the inverse was already calculated for this object, the it outputs the cached value, 
##  otherwize it calculated the inverse.
cacheSolve <- function(x, ...) {
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}