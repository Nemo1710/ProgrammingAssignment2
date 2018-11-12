## The following function is designed to avoid time-consuming computations when computing the inverse matrix.
## It allowed to retrieve cached data of the computations previosly made, or, is the new object is set,
## the new calculations will be performed.


## By the function "makecachematrix" it creates a special "matrix" object that can cache its inverse

makecachematrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, 
       get = get, 
       setsolve = setsolve, 
       getsolve = getsolve)
  }


## By the function "cachesolve" computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cachesolve <- function(x, ...) {
 m <- x$getsolve()
 if(!is.null(m)) {
  message("getting cached data")
  return(m) 
 }
 matrix <- x$get()
 m <- solve(matrix, ...)
 x$setsolve(m)
 m
 }