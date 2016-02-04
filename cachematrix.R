#This script calculate and cache inverse of a matrix and If we need 
#to recompute it later, it looked up in cache and return the value


## This function 
## set the value of a matrix
## get the value of a matrix
##set the value of the inverse
##get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  
  inv<-NULL
  set <- function(y){
    x<<- y
    inv<<- NULL
  }
  
  get<-function()x
  setinverse<-function(inverse)inv<<-inverse
  getinverse<-function()inv
  list(set = set, get= get, setinverse = setinverse,getinverse=getinverse)
}



##It checks whehter or not the inverse has already been calculated or not
##This function calculate the inverse of the matrix if not already claculated
##If already calculated then it return that value from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv<-x$getinverse()
    if(!is.null(inv)){
      message("getting cache data")
      return(inv)
    }
    data<-x$get()
    inv<-solve(data,...)
    x$setinverse(inv)
    inv
}
