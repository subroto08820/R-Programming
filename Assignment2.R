## Below are two functions that together compute the inverse
## of an invertible matrix.

## This function takes a matrix and returns a list of 4 functions
## that operate on the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function computes the inverse of the invertible
## matrix if it is not already computed and always returns the resulting matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
