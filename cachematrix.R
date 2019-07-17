## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a speial matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invMatrix = NULL
  set <- function(y){
    x <<-y
    invMatrix <<- NULL
  }
  get <- function()x
  setinvMatrix<- function(inverse) invMatrix <<-inverse
  getinvMatrix<- function() invMatrix
  list(set=set, get=get,
       setinvMatrix=setinvMatrix,
       getinvMatrix=getinvMatrix)
}


## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invMatrix <-x$getinvMatrix()
  if(!is.null(invMatrix)){ ## if invMatrix is not null
    message("getting cached data")
    return(invMatrix)
  }
  data <- x$get()
  invMatrix<-solve(data,...)
  x$setinvMatrix(invMatrix)
  invMatrix
}
