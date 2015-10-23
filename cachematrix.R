## The makeCacheMatrix() function takes an invertible matrix as an input,
## caches that matrix, and provides two member functions to get and set
## that cached matrix. The cached matrix can also cache the inverse of
## the matrix, and provides two member functions to get and set the
## inversed matrix.

## Run the cacheSolve() function with the result of makeCacheMatrix() 
## to create and cache the inverse.

## Get the cached matrix using m$get()
## Set the cached matrix using m$set()
## Get the cached inverse using m$getInverse()
## Set the cached inverse using m$setInverse()

makeCacheMatrix <- function(m = matrix()) {
        ## inverse is a variable that will store the inverse of a matrix m
        ## Initialze inverse to NULL
        inverse <- NULL
        
        ## Define four member functions - get, set, getInverse, setInverse
        
        ## get will return the stored matrix
        get <- function() {
                m
        }
        ## set will set the m variable to the input argument newMatrix?
        set <- function(newMatrix) {
                ## Assign the new matrix to the parent's m variable
                m <<- newMatrix
                ## Reset the parent's inverse variable to NULL 
                ## when a new matrix is stored
                inverse <<- NULL  ## Assignment to parent's variable
        }
        
        ## setInverse will set the inverse variable to the inversed matrix
        setInverse <- function(inversedMatrix) {
                inverse <<- inversedMatrix  ## Assignment to parent variable
        }
        
        ## getInverse will return the inverse variable
        getInverse <- function() {
                inverse
        }
        ## Return multiple objects (the member functions)
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## The cacheSolve function takes the cache matrix object (the result of
## using makeCacheMatrix()) as input.  If it has already solved and cached
## the inverse, it will return the cached inverse.  Otherwise it will access
## the cached matrix of the input, solve it, and then cache and return
## the inverse.

cacheSolve <- function(cm, ...) {
        ## Get the inverse matrix from the cacheMatrix
        inverse <- cm$getInverse()
        
        ## Return the inverse if it is cached (if it is not null)
        if(!is.null(inverse)) {
                return(inverse)
        }
        
        ## If the inverse is not cached, solve and cache
        matrixToSolve <- cm$get()
        inverse <- solve(matrixToSolve)
        cm$setInverse(inverse)
        
        ## Return the new inverse
        inverse
}