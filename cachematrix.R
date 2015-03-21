
## makeCacheMatrix
## the function makeCacheMatrix creates a list of 4 functions to
##      1) set the value of the matrix
##      2) get the value of the matirx
##      3) set the value of the inverse
##      4) get the value of the inverse
##
##
## cacheSolve
## for a given matrix x cacheSolve checks if an inverse was already calcuted. if yes the
## cached inverse will be returned. if not it will be created and returned


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## reset to NULL in case cachesolve has not been used
        y <- NULL ## reset to NULL in case  cachesolve has not been used
        setmatrix <- function(y) { #set the new matrix
                x <<- y ## caches the matrix
                m <<- NULL ## sets the value of m (the matrix inverse from cacheSolve) to NULL
        }
        getmatrix <- function() x ##this function returns the value of x (our matrix)
        setinverse <- function(solve) m <<- solve(x) ##inverting the matrix x and store in m
        getinverse <- function() m ##this function returns the value of m (our inverse)
        list(setmatrix = setmatrix, getmatrix = getmatrix, ## creates and returns a list storing the 
             setinverse = setinverse,                      ## the four subfunctions
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()                   ## check for already computed inverse
        if(!is.null(inv)) {                     ## if not NULL (already computed)
                message("Getting cached data")  ## output to UI
                return(inv)                     ## returning cached inverse
        }
        y <- x$getmatrix()                      ## get value of input matrix
                
        x$setmatrix(y)                          ## caching input matrix 
        inv <- solve(y, ...)                    ## inverting input matrix
        x$setinverse(inv)                       ## cahing inverse of input matrix
        inv                                     ## returning inverse
}
