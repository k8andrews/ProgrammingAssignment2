## Caching inverse of a Matrix
## Programming Assignment # 2, Week 3
## functions to save inverse of a matrix and calculate inverse

## Creates object to cache inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setInverse <- function() invM <<- solve(x) ##calculate inverse
  getInverse <- function() invM
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## CacheSolve function will compute inverse of makeCacheMatrix
## If inverse already calculated, will retrieve value

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invM <- x$getInverse()
  if(!is.null(invM)) {
    message("getting cached data") ##when pulling from cache
    return(invM)
  }
  data <- x$get()
  invM <- solve(data)
  x$setInverse()
  invM
  
}
