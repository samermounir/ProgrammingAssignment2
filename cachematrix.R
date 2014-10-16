## Th Code contains of two functions:
## 1- makeCacheMatrix: This function creates a special "matrix" object that  
##    can cache its inverse.


makeCacheMatrix <- function(x = matrix()) 
        # This function returns 4 functions:
        #         1-setmatrix, which sets the matrix to be used
        #         2-getmatrix, which gets the used matrix
        #         3-setinverse, which sets the inverse matrix of the used matrix
        #         4-getinverse, which gets the used inverse matrix
{ m<- NULL                    
  setmatrix <- function(y) 
  {
          x <<- y
          m <<- NULL   ## sets the inverse matrix value to NULL
          
  }
  getmatrix <- function() x   ## returns the input matrix
  setinverse<- function(tm) m <<- tm  ## sets the inverse matrix in global environment 
  getinverse <- function  ()  m  ## returns the used inverse matrix
  list ( getmatrix =getmatrix , setmatrix=setmatrix 
         ,getinverse = getinverse , setinverse =setinverse)  
}


## 2- cacheSolve: This function computes the inverse of the special "matrix"  
##    returnedby makeCacheMatrix above
cacheSolve <- function(x, ...) 
{
        m<- x$getinverse()
        ## The If Condition Checks if the input matrix is not 
        ## calculated before
        if(!is.null(m)) 
        {
                message("getting cached data")
                return(m)   ##Return the cached inverse matrix 
        }
        ## Calculates & returns a matrix that is the inverse of 'x'
        data<- x$getmatrix()
        m<- solve(data) ## solve function calculates the inverse of a matrix
        x$setinverse(m) ## calls the setinverse function 
        m
}
