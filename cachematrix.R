## The below functions calculates the inverse of a matrix, if the result is already cached, 
## it uses the cached value

## This function creates the getter and setter methods for the matrix and its inverse
## input argument is the matrix
## return value are the list of functions to get and set the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  Y<-NULL
  get<-function() X
  set<-function(Z){
    X<-Z
    Y<-null
  } 
  getinverse<-function() Y
  setinverse<-function(inverse) Y<<-inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function gets the inverse if it is cached or calculates its value and caches 
##the result if it is not

cacheSolve <- function(x, ...) {
  inv<-x$getinverse()
  if(!is.null(inv))
  {
    message("cached data")
    return(inv)
  }
  X<-x$get()
  Y<-solve(X)
  x$setinverse(Y)
  Y
}

x<-matrix(c(1,4,0,1),2,2)
a<-makeCacheMatrix(x)
## Return a matrix that is the inverse of 'x'
b<-cacheSolve(a)
