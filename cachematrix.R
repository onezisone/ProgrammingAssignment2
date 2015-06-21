## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function create matrix object
makeCacheMatrix <- function(x = matrix()) 
{
  inv <- NULL
  ##set the matrix
  set <- function(y) 
  {
    x <<- y
    inv <<- NULL
  }
  ##get the matrix
  get <- function() x
  ##set the inverse
  setinverse <- function(inverse) inv <<- inverse
  ##get the inverse
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  ##if cache is exist
  if(!is.null(inv)) 
  {
    message("getting cached data")
    return(inv)
  }
  ##get the matrix from the object
  data <- x$get()
  ##solve the inverse
  inv <- solve(data, ...)
  ##set the inverse to the object
  x$setinverse(inv)
  inv
}
