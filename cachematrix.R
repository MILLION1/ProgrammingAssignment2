## This Function makeCacheMatrix sets a matrix as an input, get the value of the matrix,  
##sets the value of the inverse of the matrix and then get the inverse of the Matrix.Here the matrix object  
##can cache its own object

makeCacheMatrix <- function(x = matrix()) {
InvOfMatrix <- NULL
  set <- function(y) {
    x <<- y
    InvOfMatrix <<- NULL
  }
  get <- function() x
  setInvOfMatrix <- function(inverse) InvOfMatrix <<- inverse
  getinverse <- function() InvOfMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}
## The function cacheSolve takes the output of the previous matrix makeCacheMatrix(matrix) as an   
## input and checks inverse of matrix from makeCacheMatrix(matrix) if it has any value in it or not.  
##In case inverse of matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from   
## and set the invertible  matrix by using the solve function.  
##In case inverse matrix from makeCacheMatrix((matrix) has some value in it (always works  
##after running the code 1st time), it returns a message  "getting Cached data"   
##and the cached object

cacheSolve <- function(x, ...) {
 InvOfMatrix <- x$getinverse()
  if(!is.null(InvOfMatrix)) {
    message("getting cached data")
    return(InvOfMatrix)
  }
  data <- x$get()
  InvOfMatrix <- solve(data, ...)
  x$setinverse(InvOfMatrix)
  InvOfMatrix
}
