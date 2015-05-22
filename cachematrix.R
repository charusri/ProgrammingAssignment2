makeCacheMatrix<- function(x = matrix()) {
  
  ##Initializing inv to NULL for a new matrix call
  inv <- NULL
  
  ##Fun Set:Allows the ability to reset the value of the matrix variable 
  # without calling the makeCacheMatrix function again
  # Additionally, reinitializes the value of the inverse to NULL
  set <- function(newmat) {
                           x <<- newmat
                           inv <<- NULL
  }
  
  ##Function get:Returns the current matrix stored in the variable
  get <- function() x
  
  ##Fun setInverse:Caches the value of the inverse into the cached variable inv  
  setInverse <- function(inverse) inv <<- inverse
  
  ##Fun getInverse: Returns the cached inverse of the matrix associated with the variable called
  getInverse <- function() inv
  
  
  ##List of internal methods in the makeCahceMatrix function so that a calling function knows
  ##what methods are available
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  #Input: mat is a matrix created using the makeCacheMatrix function
  
  #Obtain the current inverse value of the matrix passed 
  #stored in the cached variable
  inv <- x$getInverse()
  
  ##If the value returned is not NULL -
  ##then return the current cached inverse matrix
  ##The return exits the cacheSolve function
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##This part of the function is only accessed when  
  ## there is no cached inverse value for the matrix passed
  
  ##Get the value of the current matrix
  data <- x$get()
  
  ##Solve to get the Inverse
  inv <- solve(data, ...)
  
  ##Store the inverse value in the cache
  x$setInverse(inv)
  
  ##print the inverse value out
  inv
}