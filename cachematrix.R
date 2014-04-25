## Put comments here that give an overall description of what your
## functions do

## The first funtion "makeCacheMatrix" creates a special "matrix" object 
## that can cache its inverse. --> In fact, it's a litte bit the same as the makeVector funtion!!!
## The special "matrix" is a list containing functions to 

##    1. set the value of the matrix
##    2. get the value of the matrix
##    3. set the value of the inverse
##    4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  
  c <- NULL
  set <- function(y) {
    x <<- y
    c <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) c <<- inverse
  getInverse <- function() c
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## ## The following second function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## However, it first checks to see if the inverse has already been calculated (and the matrix has not changed).
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and 
## sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  c <- x$getInverse()
  if(!is.null(c)) {
    message("getting cached data")
    return(c)
  }
  data <- x$get()
  c <- solve(data, ...)
  x$setInverse(c)
  c
}
