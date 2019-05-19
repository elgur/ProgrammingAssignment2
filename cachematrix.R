## The function cacheSolve, takes in a matrix and returns its inverse and caches the result.
## This speeds up calculations, if an inverse of a matrix is used multiple times. 


## makeCahceMatrix is a function that creates an matrix object.
# The matrix object is populated with a call to the set function. 
# The matrix object chaches the pre calculated inverse with a call to setinv.
# The inverse can then be retrived with the getinv command.
# getinv returns NULL by default, if setinv has not been previously called.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y){
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setinv <- function(INVERSE) I <<- INVERSE
  getinv <- function() I
  list(set = set, get = get, 
       setinv = setinv, getinv = getinv)
}


## cahceSolve, takes in a matrix x created with makecacheMatrix above and checks if its inverse is available in cache.
# It retrives the inverse from cache if it is available, else it calculates the inverse
# and stores it in cache.

cacheSolve <- function(x, ...) {
  I <- x$getinv()
  #check if I has default value NULL, if not return I
  if((!is.null(I))){
      message("returning chaced Inverse")
      return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setinv(I)
  I
        ## Return a matrix that is the inverse of 'x'
}
