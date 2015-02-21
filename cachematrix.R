## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to
## set the value of the matrix -"set"
## get the value of the matrix -"get"
## set the value of the inverse matrix -"setinverse"
## get the value of the inverse matrix - "getinverse"

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(y) {
          x <<- y
          m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve #calculates inverse and assigns it to m
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" - "m"
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
  m <- x$getinverse() # applies the getinverse function to m
  if(!is.null(m)) {   ## checks if "m" is not null implying the inverse has already been calculated
    message("getting cached data")
    return(m)
  }
  matrix.data <- x$get() #new matrix
  m <- solve(matrix.data, ...) ## calculates inverse
  x$setinverse(m) ## sets the value of the newly calculated inverse
  m
}





