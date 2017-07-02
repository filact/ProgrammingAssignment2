## The following two functions work together to calculate the inverse of an inputted matrix.
## To save calculation time, the inverse matrix is cached and the cached result is returned if
## the inputted matrix is exactly the same as the immediately preceding input. Otherwise, the
## inverse is recalculated and the result is cached.

## The following function creates a list of functions to
## 1. Set the value of the inputted matrix
## 2. Get the value of the inputted matrix
## 3. Set the value of the inverse of the inputted matrix
## 4. Get the value of the inverse of the inputted matrix

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


## The following function returns the inverse of the matrix processed by the function above.
## If the matrix is exactly the same as before, the cached result is returned to save calculation time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
