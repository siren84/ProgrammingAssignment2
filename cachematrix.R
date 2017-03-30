## Caching the Inverse of a Matrix

## makeCacheMatrix
## ---------------
## get: returns the x stored in the main function
## set: changes the x stored in the main function
## setinverse, getinverse: store the value of the input in a variable m

## cacheSolve 
## -----------
## If the inverse of specific matrix has already been calculated  
## then cacheSolve uses the cache to get the inverse matrix. If not, 
## it gets the matrix which is stored in makeCacheMatrix (data), 
## calculates the inverse (m) 
## and it stores it in makeCacheMatrix (x$setinverse(m)).

## creating a "matrix" object that can cache its inverse

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


## Computing the inverse of the “matrix” object 

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

