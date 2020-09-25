## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL                           #initializing inverse as NULL
      set <- function(y){
        x <<- y
        inv <<- NULL
      }
      get <- function()  {x}                #function to get matrix x
      setInverse <- function(inverse) {inv <<- inverse}
      getInverse <- function() {inv}        #function to obtain inverse of matrix
      list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...)     #Gets cache data
  {
     inv <- x$getInverse()
     if(!is.null(inv)){           #Checking whether inverse is null
         message("getting cached data")
         return(inv)              #Returns inverse value
     } 
     mat <- x$get()
     inv <- solve(mat,...)        #Calculates inverse value
     x$setInverse(inv)
     inv                          ## Return a matrix that is the inverse of 'x'
      
}
