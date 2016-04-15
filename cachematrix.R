## Put comments here that give an overall description of what your
## functions do

## Variable x: a square matrix
## Returns a List of functions
##  set: sets matrix to variable x and
##       sets inv to NULL 
##       '<<-' assigns value to object in different environment
##  get: returns matrix assigned to variable x
##  setInverse: sets the inverse of matrix to variable inv
##  getInverse: returns value assigned to inv
##
## List used in cacheSolve()


makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <-- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Variable x: matrix, vector 
## Returns inverse of input matrix 'x'

cacheSolve <- function(x, ...) {
  
  ## check if inverse of 'x' has been solved
  inv <- x$getInverse()
  if(!is.null(inv)){
    ## returns inverse of 'x'
    message("getting cached data")
    return(inv)
  }
  
  ## solves for inverse of 'x'
  ## assign to variable inv
  matX <- x$get()
  inv <- solve(matX, ...)
  ## cache inverse of 'x' using setInverse function
  ## returns inverse of matrix 'x'
  x$setInverse(inv)
  return(inv)

}
