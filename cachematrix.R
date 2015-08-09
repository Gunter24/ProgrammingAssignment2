## Following functions store matrix & its inverse in cache using <<- operator,
## fetch the inverse from cache if its already calculated otherwise compute it & store in cache for future

## makeCacheMatrix():
## set - stores the matrix in cache
## setI - stores matrix inverse in cache
## get - fetches matrix
## getI - fetches matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setI <- function(I) m <<- I
  getI <- function() m
  list(set = set, get = get,
       setI = setI,
       getI = getI)
}


## cacheSolve(): If matrix remains unchanged & its inverse has already been computed, then fetches inverse from cache
## 					otherwise, re-computes the new inverse, stores it in cache & prints it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'		
  m <- x$getI()
  if(!is.null(m)&&m==solve(x$get())) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setI(m)
  m
}
