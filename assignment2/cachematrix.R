## Create a matrix object which caches its inverse once calculated
## with the cacheSolve function

## Examples:
#  m <- makeCacheMatrix(matrix(1:4, 2, 2)) 
#  m$get()
#  m$set(c(2, 3, 2, 2), 2, 2)
#  m$getInverse()
#  (don't use the m$setInverse() directly!)
#  cacheSolve(m)


# Create the "cached matrix" object
# 
# Args:
#  x: a numerical matrix which is invertible
#
# Returns:
#  the list of methods (get, set, getInverse, setInverse) applicable to the object
#
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
    
    inv <<- NULL  #why? 
    
    # return the available methods for makeCacheMatrix
    list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}


# Return the inverse of the matrix, either by calculating the value 
#   or retrieving it from a cache. When the solution is calculated,
#   it is cached so it doesn't need to be re-calculated.
#
# Args:
#  x: the cached matrix object
#
# Returns:
#  The inverse of the matrix inside x
#
cacheSolve <- function(x, ...) {
    
    # check for available inverse for matrix
    inv <- x$getInverse()
    
    # if we don't have a solution, solve and cache
    if (is.null(inv)) {
        solve(x$get()) -> inv
        x$setInverse(inv)
    } 
    
    ## Return a matrix that is the inverse of 'x'
    inv
    
}
