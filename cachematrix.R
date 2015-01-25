## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makecacheMatrix takes in an argument of type matrix() and returns a list of get, set, getInvMatrix and setInvMatrix functions. 
## set() and setInvMatrix() cache matrix passed as arguments.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    get <- function() x
    
    setInvMatrix <- function(invMatrix) {
      m <<- invMatrix }
    getInvMatrix <- function() {
      m
    }
    
    ## getevn <- function() environment()
    
    list(set = set, get = get, 
         setInvMatrix = setInvMatrix, 
         getInvMatrix = getInvMatrix)
    
}


## Write a short comment describing this function
## This is a wrapper function that takes as argument the list of functions returned by makeCacheMatrix
## The function checks if Inverse of the Matrix exists as a cached value. If it does, it returns teh cached inv matrix
## If not - it caclulates the inv of the matrix using Solve() function. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInvMatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInvMatrix(m)
  m
  
}
