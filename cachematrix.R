## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(invmat) inv <<- invmat
  getinv<-function() inv
  list(set=set,get=get,setinv = setinv,getinv=getinv)
}


## This function returns the inverse of the matrix x and retrieves the inverse if it is already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat<-matrix()
  mat<-x$getinv()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data<-x$get()
  invmat<-solve(data)
  x$setinv(invmat)
  invmat
}
