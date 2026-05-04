## Put comments here that give an overall description of what your
## functions do
#  We want to avoid heavy computations when possible. Here is the case of 
#  matrix inversion, leveraging solve(x) from R, where x is a n*n matrix.
# 
# Here, there won't be any checks on whether the matrix is squared. It will 
#  have 2 functions described below:
#  `makeCacheMatrix`: This function creates a special "matrix" object
#                     that can cache its inverse.
# `cacheSolve`: This function computes the inverse of the special
#               "matrix" returned by `makeCacheMatrix` above. If the inverse has
#               already been calculated (and the matrix has not changed), then
#               `cacheSolve` retrieve the inverse from the cache.
#
# IMportant: the `<<-` operator that assigns a value to an object in an 
#             environment that is different from the current environment
#             is used.

## makeCacheMatrix
# it creates a special matrix object (within a list) 
#                     that can cache its inverse. 
# it is really a list containing a function to
# 
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse matrix
# 4.  get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  # m will be the inverse matrix 
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(inputm) m <<- inputm
  getinvmatrix <- function() m
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}


## cacheSolve
# This function computes the inverse of the special "matrix" 
# returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` retrieve the inverse from the cache. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinvmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data_m <- x$get()
  m <- solve(data_m, ...)
  x$setinvmatrix(m)
  m
}
