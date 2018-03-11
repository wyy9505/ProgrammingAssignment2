## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  # Defining a set() to assign new value of matrix
  set <-function(m){
    x<<-m
    inverse<<-NULL
  }
  # Defining a get() to retrieve the value of the matrix
  get <- function() x
  setinverse <-function(inv) inverse<<-inv
  getinverse <-function() inverse
  list(set = set,get = get, setinverse=setinverse,getinverse=getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinverse()
  # If the data is already available in the cache, return it
  if(is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  # Computing the inverse of x
  data<-x$get()
  inv<-solve(data)
  x$setinverse(inv)
  inv
    
}
