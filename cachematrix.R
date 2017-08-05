## As inverse matrix operations are expernsive, if you are going to need it more than once its
## better to store as part of a matrix object in a cache for access later on

## Creates a Matrix object with getters and setters and can store the matrix and the inverse matrix in the cache

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Returns the existing cached value of the required matric inverse if available before attempting to create the inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinverse(i)
        i
}
