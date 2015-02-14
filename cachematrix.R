## The first function creates a special "matrix object" that can cache its inverse.
## The second function then checks whether the inverse has already been computed;
## if so, it uses the stored version. Otherwise it computes the inverse.

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  seti <- function(solve) i <<- solve
  geti <- function() i
  list(set = set, get = get,
       seti = seti,
       geti = geti)
}


## The following function calculates the inverse of the special "matrix" 
## created with the above function. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the
## matrix and sets the value of the inverse in the cache via the "seti" function.

cacheSolve <- function(x, ...) {
  i <- x$geti()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$seti(i)
  i
}
