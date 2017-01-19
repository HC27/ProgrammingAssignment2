## Functions to cache matrix inversing, will store the last inverse in the cache
#and either return this or if the matrix has changed compute and store a new
#inverse

## creates a special matrix which stores its own inverse in the cache

makeCacheMatrix <- function(x = matrix()){
  i <- NULL
  set <- function(y){
    x <<-y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) i<<-inv
  getinverse <- function() i
  list(set=set, get=get,
       setinverse=setinverse, getinverse=getinverse)
}

## uses makeCacheMatrix to eitehr compute teh inverse of a matrix or retrieve it
##from the cache if this has already been calculated

cacheSolve <- function(x, ...) {
  y <- x$get()
  #if(y==x){
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
    # } 
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}