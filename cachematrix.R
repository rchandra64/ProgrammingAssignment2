## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix - Defines the functions to keep each matrix and its inverse in the in-memory cache.
# cacheSolve - Receives the matrix X as an input parameter, checks the cache for its inverse matrix
#   and returns if it exists otherwise, calculate the inverse matrix and sets it in the cache.

## Write a short comment describing this function
# makeCacheMatrix::set() sets the matrix.
# makeCacheMatrix::get() returns the matrix to the caller
# makeCacheMatrix::setInverse() sets the inverse matrix of the x
# makeCacheMatrix::getInverse() returns the inverse matrix of the x
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <-function(y){
    x <<-y
    inverseMatrix <<-NULL
  }
  
  get <- function() x
  getInverse <- function() inverseMatrix
  setInverse <- function(inverseMatrixUsingSolve)  inverseMatrix <<- inverseMatrixUsingSolve
  list(set=set, get=get, getInverse=getInverse, setInverse=setInverse)
}


## Write a short comment describing this function
# First it checks whether the inverse matrix of the matrix x is in cache.  
# This is done by calling the getInverse() subfunction of makeCacheMatrix function.
# If the inverseMatrix does exist in the cache already, it will be returned.
# If the inverseMatrix value is NULL, it means the cache doesn't have the value.
# Using the R built-in solve(), calculate the inverse of the matrix x and call setInverse() sub function in makeCacheMatrix to set the value.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)){
    message('Getting the inverse matrix from cache')
    return(inverseMatrix)
  }
  
  #If you're here, this is the first time we are seeing this matrix.
  # Calculate the inverse using solve(x)
  inverseMatrix <- solve(x$get())
  #Set the inverseMatrix in the cache
  x$setInverse(inverseMatrix)
  #Return the value
  inverseMatrix
}


#Test the functions
#x1 <- matrix(c(1:4), nrow = 2, ncol = 2, byrow = TRUE)
#matrixCache1 <- makeCacheMatrix(x1)
#inverseMatrix1 <- cacheSolve(matrixCache1)

##The second time, we should see the value in cache and the appropriate message.
#cachedInverseMatrix1 <- cacheSolve(matrixCache1)
#cachedInverseMatrix1

##Another matrix with different values
#x2 <- matrix(c(7:10), nrow = 2, ncol = 2, byrow = TRUE)
#matrixCache2 <- makeCacheMatrix(x2)
#inverseMatrix2 <- cacheSolve(matrixCache2)

##The second time, we should see the value in cache and the appropriate message.
#cachedInverseMatrix2 <- cacheSolve(matrixCache2)
#cachedInverseMatrix2
