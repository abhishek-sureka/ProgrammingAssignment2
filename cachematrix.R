
## This function returns a list of four functions, which are used to set the value
## of the matrix, get the value of the matrix, set the inverse of the matrix
## and get the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function()x
  setinverse<-function(inverse)inv<<-inverse
  getinverse<-function()inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)

}

## This function calculates the inverse of the matrix. But before doing so, it
## checks if the inverse has already been calculated previosuly and stored in 
## the cache. If yes, it returns the value from the cache, else it computes the
## inverse and stores the result in cache.
cacheSolve <- function(x, ...) {
        inv<-x$getinverse()
        if(!is.null(inv)){
          message("Getting cached inverse")
          return(inv)
          
          
        }
        matrix<-x$get()
        inv<-solve(matrix)
        x$setinverse(inv)
}
