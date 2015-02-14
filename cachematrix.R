## The following functions cache the inverse of a matrix and retrieve it.


## This function creates a special "matrix" object that can cache its inverse.
## It creates a list containing functions to :
## 1. set the value of the matrix
## 2. get the value of the matirx
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y){
                x <<-y
                Inv <<- NULL
        }
        get <- function() x
        setInv <- function(Inverse) Inv <<- Inverse
        getInv <- function() Inv
        list( set = set, get = get, setInv = setInv, getInv = getInv)
}

## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## x is a special 'matrix' returned by makeCacheMatrix above.
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getInv()
        if(!is.null(Inv)){
                message("getting cached data")
                return(Inv)
        }
        data <- x$get()
        Inv <- solve(data, ...)
        x$setInv(Inv)
        Inv
}




