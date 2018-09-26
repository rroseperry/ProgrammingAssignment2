## These two functions satisfy (in part) the assignment for week three
## They cache the inverse of a matrix to save computations

## MakeCacheMatrix takes a matrix as input, takes the inverse and saves as
## as a variable. That variable is passed to cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL     #initializes v in code, will be used later
  
  set <-function(y){
    x <<- y     #sets x to input which is a sqaure matrix
    v <<- NULL  #sets m to NULL
  }
  get <- function()x  #lexical scoping makes x accessible to get
  setinverse <- function(solve) v <<- solve
  getinverse <- function() v
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function looks for the inverse to entered matrix, computes if nec.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  v <-x$getinverse()
  
  if(!is.null(v)){
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data)
  x$setinverse(v)
}
