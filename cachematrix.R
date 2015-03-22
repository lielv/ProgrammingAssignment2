## Put comments here that give an overall description of what your
## functions do

## The function create a list containing a matrix, it's inverse (set to null) 
## and the function setinv and getinv for setting and getting the inverse

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


## The function gets a CacheMatrix variable and returns its inverse. If the inverse
## was already calculated it getting it from the cache, else it calculte it.

cacheSolve <- function(x, ...) {
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
