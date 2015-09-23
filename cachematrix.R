## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list containing functions to 
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the inverse of the matrix
## 4- get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix ()) {
  m<- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinv <- function(inv) m <<- inv
  
  getinv <- function() m
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calculates the inverse of a matrix : if the 
## inverse has been already calculated, there will be a 
## message "getting cached data" and the value will be 
## returned. If not, it calculates the inverse of the matrix ## and sets the value of the inverse in the cache via the 
## setinv function 

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
