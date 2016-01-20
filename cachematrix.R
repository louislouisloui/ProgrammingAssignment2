## Put comments here that give an overall description of what your
## functions do

## This function creates 4 functions to set and get the value of the matrix, 
## and to set and get the inverse of the matrix
## it uses the <<- to force to assign a value over the value assigned in the parent frame

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function (y){
    x<<-y
    m<<-NULL
  }
  
  get <- function () x
  setinv <- function (inverse) inv <<- inverse
  getinv <- function () inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## It receives the list of function to access to the cached value of x
## First it tests if there is already a inv cached, in this case in returns it
## otherwise it calculates the inverse and cache it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
}
