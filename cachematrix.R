## Put comments here that give an overall description of what your
## functions 
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function() x                                        #get the value of the matrix
  setInverse <- function(inverse) invMatrix <<- inverse            #set the value of the invertible matrix
  getInverse <- function() invMatrix                               #get the value of the invertible matrix 
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setinverse = setinverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  #get the value of the invertible matrix from the makeCacheMatrix function
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) {                                    #if inverse matrix is not NULL
    message("getting cached Invertible Matrix")                #type message :  getting cached invertible Matrix                            
    return(invMatrix)                                          #return the invertible matrix
    
  }
  # if value of the invertible martrix is NULL then
  MatrixData <- x$getMatrix()                                   #get the original matrix data
  invMatrix <- solve(MatrixData, ...)                           #us solve function to inverse the matrix
  x$setInverse(invMatrix)                                       #set the invertible matrix
  invMatrix                                                     #return the invertible matrix
}    ## return a matrix that is the inverse of 'x'

