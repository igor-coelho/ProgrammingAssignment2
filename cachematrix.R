## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# set, get, set inv, getinv
library(MASS) #MASS -- calculate inverse of non squared and square matrices

makeCache <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse)inv <<- inverse
  getinv <- function() {
    inver<-ginv(x)
    inver%*%x
  }
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Write a short comment describing this function
# Used to get the cache data

cachesolve <- function(x, ...) {
  inv<- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

