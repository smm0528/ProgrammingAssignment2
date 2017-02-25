## Put comments here that give an overall description of what your
## functions do
##test
##> r<-matrix(c(1,-1,1,-2),2,2)
##> r
##    [,1] [,2]
##[1,]    1    1
##[2,]   -1   -2
##> solve(r)
##   [,1] [,2]
##[1,]    2    1
##[2,]   -1   -1
##> m<-makeCacheMatrix(r)
##> cacheslove(m)##    [,1] [,2]
##[1,]    2    1
##[2,]   -1   -1
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inver) inverse <<- inver
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinverse(inver)
  inver

}
