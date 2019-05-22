## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# This function takes a square matrix as an argument 
# x = matrix(c(vector elements), nrow = length(vector)^0.5, ncol = length(vector)^0.5))
# and creates a special objec that stores a matrix and caches its inverse 


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

# This function checks the cache for the inverse matrix
# If not there, it calculates the inverse of the special matrix sets the value of the inverse in the cache via the setinverse function
# If there, it gets the inverse from the cache and skips the calculation

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

a <- makeCacheMatrix(x = matrix(c(2,4,4,62,74,12,4,62,1), nrow = 3, ncol = 3))
b <- cacheSolve(a)

