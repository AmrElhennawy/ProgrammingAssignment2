## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix())
{
    InverseX <- NULL
    Set <- function(y = matrix())
    {
      x <<- y            # x is the input matrix
      InverseX <<- NULL  # clear any cached value for the old matrix
    }
    Get <- function() x    # x is retrieved from the parent environment
    SetInverse <- function(MatrixInverse) InverseX <<- MatrixInverse
    GetInverse <- function() InverseX
    list(Setter = Set, Getter = Get,
         SetInverse = SetInverse,
         GetInverse = GetInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...)
{
    ## Return a matrix that is the inverse of 'x'
    InverseX <- x$GetInverse()
    if(!is.null(InverseX))      # There is a chacehd inverse
    {
      message("getting cached data")
      return(InverseX)
    }
    data <- x$Getter()     # Get the whole matrix
    InverseX <- solve(x$Getter())
    x$SetInverse(InverseX)
    InverseX
}
a<- makeCacheMatrix(matrix(c(1,2,3,4), 2,2))
cacheSolve(a)
cacheSolve(a)
