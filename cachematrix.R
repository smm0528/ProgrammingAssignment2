
##Test
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
##The first function, makeCacheMatrix creates a special "Matrix", which is really a list containing a function to

## 1 set the value of the matrix
## 2 get the value of the matrix
## 3 set the value of the inverse
## 4 get the value of the inverse

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


## The following function calculates the inverse of the special "matrix" 
##created with the above function. However, it first checks to see 
##if the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse of the
## data and sets the value of the inverse in the cache via the setinverse function.

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
