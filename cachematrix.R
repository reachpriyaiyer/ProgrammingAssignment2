
## This function describes the four functions to be used by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  #initializing the object where the inverse matrix is going to be assigned
  inv<-NULL
  #Defining the set function which is used to clear the values of matrix x and store the new matrix 
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  #the cacheSolve uses this get function to get the matrix for which inverse needs to be calculated
  get<-function() x
  #cacheSolve function uses the setinv function to cache the calculated inverse matrix
  setinv<-function(invmat) inv <<- invmat
  #cacheSolve uses the getinv function to retrieve the values of the cache
  getinv<-function() inv
  #finally the function makeCacheMatrix returns all four functions as a list
  list(set=set,get=get,setinv = setinv,getinv=getinv)
}



## This function returns the inverse of the matrix x and retrieves the inverse if it is already cached

cacheSolve <- function(x, ...) {
  mat<-matrix()
  # This line retrieves the value of the cache which is the inverse of the matrix
  mat<-x$getinv()
  #Checks if the cache is not empty and if so prints the inverse matrix
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  #if the above if statement fails, then it executes the else part which fetches the matrix, calculates inverse, 
  #puts the inverse matrix in the cache and then returns the inverse matrix
  else {
    data<-x$get()
    invmat<-solve(data)
    x$setinv(invmat)
    invmat
  }
}