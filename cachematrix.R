## The function "makeCacheMatrix" takes an argument of type "matrix" and returns a list of specific functions related
## to the input matrix. Thus creating a special matrix with certain attributes.

makeCacheMatrix <- function(x = matrix()) 
{
  
  i <- NULL

    setMatrix <- function(y) 
    {
        x <<- y
        i <<- NULL
    }
  
  
    getMatrix <- function()
    {
        x
    } 
  
    setInverse <- function(inverse)
    {
        i <<- inverse
    } 
  
    getInverse <- function()
    {
        i
    } 
  
    list(setMatrix= setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)

}


## The second function "cacheSolve" takes an argument which is the special matrix (created using the "makeCacheMatrix" function).
## The function determines via an "if" statement whether the inverse of the special matrix has previously been computed
## by checking the "cache", if so then the inverse is returned; else, it is calculated using the "solve" function and the sub-functions
## "setInverse" and "getInverse" within the function "makeCacheMatrix".


cacheSolve <- function(x, ...)  ## Return a matrix that is the inverse of 'x'
{
    i <- x$getInverse()

    if(!is.null(i)) 
      {
      message("getting cached data")
      return(i)
      }
    
    data <- x$getMatrix()
    i <- solve(data)
    x$setInverse(i)
    x$getInverse()
}
