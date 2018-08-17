## These functions take in a matrix and solve the inverse


## This function takes a matrix and caches the inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set<- function(y) {
    x<<- y
    inv<<- NULL
  }
  get<- function () x
  setinv<- function(inverse) inv<<- inverse
  getinv<- function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}


## This function checks to see if the inverse of the matrix is cached
## If not, it solves the inverse of the matrix

cacheSolve <- function(x, ...) {
  inv<- x$getinv()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
    
  }
  data<- x$get()
  inv<- solve(data,...)
  x$setinv(inv)
  inv
  
}
