## These functions generate functions that invert a given invertible matrix, then cache it... if the operation has been done before the
## the function pulls and prints the previous result.

## invert and cache the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinverse <- function(solve)m <<- solve
        getinverse <- function()m
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## checks if the inverse of the matrix entered has previously been cached, prints result if yes, goes through the operation if no

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
