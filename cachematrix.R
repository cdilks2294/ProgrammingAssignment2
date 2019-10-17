## These functions calculate the inverse of a matrix. The first function works
## within the second. It generates a list of functions within a saved obejct.
## This output takes in a square matrix and outputs a list object with functions to 
##calculate and solve the inverse of the square matrix. 

makeCacheMatrix <- function(x = matrix()) {
  #Set m to null
  m <- NULL
  set <- function(y) {
    x <<- y 
    m <<- NULL
  }
  #From course websites example of calculating mean modified for the solve function
  get <- function() x 
  setsolve <- function(solve) m <<- solve
  getsolve <- function () m
  list(set = set, get = get, 
       setsolve = setsolve,
       getsolve = getsolve)
  
}

## This function checks if the inverse of the matrix previously generated
##has previously been calculated by the function and if it has it will print that
##along with a message stating the matrix comes from the cache. If it does not exist
## the function calculates the inverse of the matrix. 

cacheSolve <- function(x, ...) {
  #From course websites example of calculating mean modified for the solve function
  m <- x$getsolve()
  if(!is.null(m)){
    message("Cache is available")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
