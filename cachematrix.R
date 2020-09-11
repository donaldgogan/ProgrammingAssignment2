## Put comments here that give an overall description of what your
## functions do

## this function is used to create a matrix that can cahe its reverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { #set the value of the matrix in an object environment
    x <<- y
    m <<- NULL
  }
  get <- function() x #get the value of the matix
  setinv <- function(inverse) m <<- inverse #set the value of the inverse of the matix
  getinv <- function() { #get the value of the inverse of the matix
    invers <- solve(x)
  }
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## this function is used to calculate the inverse of the matrix
## the matrix should be (2,2)
## the first step is to check if the inverse is already calculate with the if condition
## else if it applied the getinv function that calculate the inverse

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

