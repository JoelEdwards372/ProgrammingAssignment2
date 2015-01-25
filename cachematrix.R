## Objective:Functions makeCacheMatrix and cacheSolve are used to find the inverse 
## of a matrix x. Condition: If the inverse has already been found, it is returned 
## without the need to recalculate the inverse.

## --------------------------------------------------------------------------------
## This function creates a special R object that 
## 1. Initializes a variable 'm';
## 2. Provides function get() to obtain "raw" matrix (of which one needs to find 
##    its inverse);
## 3. Provides function setInvmatrix() to assign computed inverse matrix (of x) to m;
## 4. Provides function getInvmatrix() to obtain the cached inverse matrix.
## --------------------------------------------------------------------------------
makeCacheMatrix <- function(x=matrix()) {
  m <- NULL
  get <- function() x
  setInvmatrix <- function(Invmatrix) m <<- Invmatrix
  getInvmatrix <- function() m
  
  # return a list of functions as an R object
  list(get=get, setInvmatrix=setInvmatrix, getInvmatrix=getInvmatrix)
}



## --------------------------------------------------------------------------------
## This function does the calculates the inverse of matrix x.  
## (1) Checks if the in-verse matrix has been found. If yes then returns from m, 
## otherwise a calculation is formed, saved to cache and returned. 
## NOTE: argument x for this function must be cached, i.e. a list returned from
## calling makeCacheMatrix(x).
## --------------------------------------------------------------------------------
cacheSolve <- function(x) {
  m <- x$getInvmatrix()
  if(!is.null(m)){
    message("Cached data found. Getting result.")
    return(m)
  }
  else {
    message("No cached data found. Calculating inverse now")
    data <- x$get()      # gets matrix from object x
    m <- solve(data)     # finds inverse matrix
    x$setInvmatrix(m)    # assigns resulting inverse matrix to object x
    return(m)
  }
}


##Tests
a <- makeCacheMatrix(matrix(1:4,2))
a$get()
a$getInvmatrix()
a$set(matrix(5:8,2))
a$get()
cacheSolve(a)
cacheSolve(a)
a$getInvmatrix()
b = a$getInvmatrix()
a$get() %*% b
