## These functions calculate the inverse of a matrix (given that the matrix can be inversed) 
## and this inverse matrix is put in a cache. When running a time consuming script this inverse
## matrix can be taken out of the cache without having te be calculated each time. 
## Krista Overvliet, September 2016


## makeCacheMatrix makes a list of functions that handle with the cached inverse matrix
## set sets the original matrix in the cache and resets the inversematrix,  
## get takes the original matrix out of the cache 
## setsolve sets the inverse matrix in the cache, 
## getsolve can take it out 

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x 
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## cacheSolve checks whether there is a cached inverse matrix,
## if yes it takes it out
## if not it will calculate it and put it in the cache
## but returns 
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
