## Caching The Inverse of a Matrix
## Matrix inversion is usually costly computation and there may 
## some benefit to caching the inverse of a matrix rather than 
##Computing  it reapeatedly.So a makeCacheMatrix is created to
##to make a special "matrix" object that can cache its inverse.
##Another function , cacheSolve is created to compute the inverse
## of the special matrix returned by the makecacheMatrix function.

## Creates A Special "Matrix" Object That Can Cache Its Inverse

makeCacheMatrix <- function(x = matrix()) {
        
        i <- NULL 
        
        set <- function(y){
                
                x <<- y
                i <<- NULL
                
        }
        
        get <- function()x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        

}


## Computes The Inverse Of The Special "Matrix" Returned By makeCacheMatrix Function.If The  
## Inverse Has Already Been Calculated Then The cacheSolve Should Retrieve Inverse From Cache

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getInverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        i<- solve(data,...)
        x$setInverse(i)
        i
        
}
