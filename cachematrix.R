## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    # get: return the matrix itself
    get <- function() {
        x
    }
    
    # set: set the matrix to given matrix `m` and clear the inverse
    #       since it's probably no longer the correct value
    set <- function(m) {
        x <<- m
        inv <<- NULL
    }
    
    # getinverse: return the inverse matrix
    getInverse <- function() { 
        inv 
    }
    
    # setinverse: set the inverse to given matrix `m`
    setInverse <- function(m) {
        inv <<- m
    }
    
    inv <<- NULL  #why? and why not <<- ?

    # return the available methods for makeCacheMatrix
    list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    # check for available inverse for matrix
    inv <- x$getInverse()
    
    # if we don't have a solution, solve and cache
    if (is.null(inv)) {
        solve(x$get()) -> inv
        x$setInverse(inv)
    } else {
        print("inverse cached")
    }
    
    ## Return a matrix that is the inverse of 'x'
    inv
    
}
