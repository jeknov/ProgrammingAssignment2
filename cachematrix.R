## makeCacheMatrix function creates a special matrix, which:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse
## 4. gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {    # set the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x     # get the value of the matrix
  setinverse <- function(inverse) inv <<- inverse   # set the value of the inverse
  getinverse <- function() inv                      # get the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function calculates the mean of the special matrix 
## created with the makeCacheMatrix function.
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {               # check if the inverse has already been calculated
    message("getting cached data")  # get the inverse from the cache
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)      # calculate the inverse of the data
  x$setinverse(inv)       # set the inverse in the cache
  inv
}