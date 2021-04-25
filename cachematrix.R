#create function for cache matrix and set a null inverse and create a set function for y
makeMatrix <- function(x = matrix()){
  invi <- NULL
  set1 <- function(y){
    x < y
    invi <<- NULL
  }
  
  #create a get function for x
  get1 <- function() {x}
  
  #create a setinverse and getinverse function
  setInverse1 <- function(inverse1) {inv <<- inverse1}
  getInverse1 <- function() {inv}
  list(set1 = set1, get1 = get1, setInverse1 = setInverse1, getInverse1 = getInverse1)
}

#the cacheSolve1 will now print the output
cacheSolve1 <- function(x, ...){
  inv <- x$getInverse1()
  if(!is.null(inv)){
    message("cached data will be fetched")
    return(invi)
  }
  mat1 <- x$get1()
  invi <- solve(mat1, ...)
  x$setInverse1(inv1)
  invi
}
