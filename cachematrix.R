##Functions to store a matrix and its inverse in the environment
##of the function makeCacheMatrix.
##The inverse is created only once

##creates a list with functions to manipulate the matrix "x"
##the matrix "x" is stored in the environment of the function

makeCacheMatrix <- function(x = matrix()) {
  inv=NULL
  set <- function(M){
    x <<- M
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list( set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


##This function returns the inverse of the matrix
##and stores it in the environment of makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  if(!is.null(inv)){
    message('getting cached inverse')
    return(inv)
    
  }else{
    M <- x$get()  #get matrix
    inv <- solve(M)
    x$setInverse(inv) #store inverse
    return(inv) 
  }
}
