## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
##repeatedly

##This function creates a special "matrix" object that can cache its inverse.
##It contains the list of functions 
## set the matrix (set)
## get the matrix (get)
## inverse the matrix using solve function (setinvmatrix)
## get the inversed matrix (getinvmatrix)

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ##set the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  ##get the matrix
  get <- function() x
  ##set the inversed matrix
  setinvmatrix <- function(solve) m <<- solve
  ##get the inversed matrix
  getinvmatrix <- function() m
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
              
}

##This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then cacheSolve should retrieve the 
##inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getinvmatrix()
  ##checking for cached data
  if(!is.null(m)){
    message("Getting Cached Data")
    return(m)
  }
  ##getting matrix
  matrix <-x$get()
  ##computing inversed matrix using solve
  m<-solve(matrix,...)
  x$setinvmatrix(m)
  m
}
