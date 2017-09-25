## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##"The function is used to assign addresses to the varirable, which mean creating pointers to 
##the variable's value and the inverse. If the inverse is calculated later, the inverse will be saved
##according to the pointer."

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y){
    x <<- y
    i <- NULL
    
  }
  
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get, 
       setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function

##"It reads the inverse from the list first, if the inverse is empty, the function 
##will calculate the inverse and cache it into the list. Else, the function will just display the inverse 
##if the inverse value exist."

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}
