## The function is an alternate to ths solve() built-in to R 
## It creates a cache for the value of the inverse matrix so that it can be reused later without
## using any extra resources

## Make a squaren matrix using the makeCacheMatrix()  function and save it under any name
## Then use cacheSolve to calculate its inverse. 
## any subsequent call of the cacheSolve of the vector will be loaded from cache
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
