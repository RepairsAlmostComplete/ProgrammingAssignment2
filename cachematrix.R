## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
## This function sets several embedded functions in the "special martix"
## 1. the set() embedded function it sets the value of the matrix
## 2. the get() embedded function retrieves the value of the matrix
## 3. the setInverse() embedded function sets the inverse of the matrix
## 4. the getInverse() embedded function retrieves the inverse of the matrix

makeCacheMatrix <- function(val = matrix()) {
    ## Creates a special matrix object that can cache its inverse
    ## val - input matrix
    
    invVal <-  NULL
    
    ## Set the value
    set <- function(setVal) {
        val <<- setVal
        invVal <<- NULL
    }
    
    ## Return the value
    get <- function() val
    
    ## Set the inverse of the matrix
    setInv <- function(solve) invVal <<- solve
    
    ## Return the cached inverse of the matrix
    getInv <- function() invVal
    
    ## List the functions
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cachesolve retrieves the inverse from the
## cache.
##
## The function uses the solve() function to obtain the inverse of the matrix.

cacheSolve <- function(val, ...) {
    ## Return a matrix that is the inverse of 'val'
    ## val - An input makeCacheMatrix variable
    ## ... - Optional arguments for the solve function
    
    ## Get the value of the inverse of the matrix
    invVal <- val$getInv()
    
    ## Check if the inverse of the matrix has been set, if it has then return
    ## the value.
    if(!is.null(invVal)) {
        message("getting cached data")
        return(invVal)
    }
    
    ## Else set the value of the inverse matrix
    matVal <- val$get()
    invVal <- solve(matVal, ...)
    val$setInv(invVal)
    invVal
}