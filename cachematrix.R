## Below are two functions that are used to create a special object that 
## stores a numeric matrix and caches its inverse

##creates a special "matrix" that can cache its inverse, which is really a list containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  mat <- matrix(nrow = 0, ncol = 0)
  set <- function(y) {
    x <<- y
    mat <<- matrix(nrow = 0, ncol = 0)
  }
  get <- function() x
  setinv <- function(inv) mat <<- inv
  getinv <- function() mat
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function calculates the inverse of the special "matrix" created with the 
## above function. However, it first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse
## in the cache via the setinv function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getinv()
  if(nrow(mat)>0) {
    message("getting cached inverse")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setinv(mat)
  mat
}