## The combination of makeCacheMatrix and cacheSolve allow the user to 
## calculate the inverse of a matrix and store that for later reference
## without needing to re-perform the calcations
## 
##
## Example Use:
## > q<-matrix(2:5,nrow=2,ncol=2)
## > q
## [,1] [,2]
## [1,]    2    4
## [2,]    3    5
## > solve(q)
## [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
## > r<-makeCacheMatrix(q)
## > cacheSolve(r)
## [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
## > r$getinverse()
## [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1


## makeCacheMatrix creates an object to store the cached inverse matrix
## and provides functions to access both the original matrix (get & set)
## and the inverse matrix (getinverse & setinverse)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) ##When changing the matrix 
  {
    x <<- y ##cache newly provided matrix into x
    m <<- NULL  ##reset cached inverse matrix to be NULL forcing CacheSolve to recalculate the Solve function
  }
  get <- function() x ##get returns the matrix x cached by the set function
  setinverse <- function(solve) m <<- solve  ##takes parameter solve and applies it to m in the parent environment would be better calling this something else!
  getinverse <- function() m ##returns the cached inverse matrix m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) ## return the object as list of functions
}


## cacheSolve takes an object created with makeCacheMatrix and checks the 
## cached inverse to see if it exists (ie it's not null) and returns the
## cached matrix if it does.  When the cached matrix does not exist (ie it
## is null) the solve function is called and the result loaded back into
## the passed objects cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) ## check if the inverse already exists and return it
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get() ## retrieve the matrix
  m <- solve(data, ...) ## calculate the inverse
  x$setinverse(m) ## store the inverse
  m ## return the inverse
}
