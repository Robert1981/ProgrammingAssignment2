# makeCacheMatrix is a function that creates a special "matrix" object 
# that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {

  i <- NULL 
 
  # Set the value of the matrix 
  set <- function(y) { 
    x <<- y 
    i <<- NULL 
  }

  # Get the value of the matrix 
  get <- function() x 
    
  # Set the value of inverse of the matrix     
  setinverse <- function(inverse) i <<- inverse 
    
  # Get the value of inverse of the matrix 
  getinverse <- function() i 
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}    


# cacheSolve is a function that computes the inverse of the special 
# "matrix" returned by  makeCacheMatrix  above. The first part of the 
# function checks if the inverse has already been calculated. If it has
# been calculated then it gets the result and skips the computation.
# If it has not been calculated then cacheSolve retrieves the 
# inverse from the cache via setinverse function.


cacheSolve <- function(x, ...) {
 
  i <- x$getinverse() 

  if(!is.null(i)) { 
    message("getting cached data") 
    return(i) 
    }

    data <- x$get() 
    i <- solve(data)
    x$setInverse(i) 
    i
} 
