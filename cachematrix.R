## This function creates a "cache" matrix. This list of matrices is what will be
## checked against in order to make sure that a solution does not already exist
## and has been computed already.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)      ## This is the list "housing" the inverted
}




## This function checkes the list of cache matrices checking to see if an
## inverse of the given matrix already exists. If it does it pulls that matrix
## inverse from the list and reports that it is doing so.
## if that matrix solution does not already exist then it simply solves for the
## inverse

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {                         ## Check for existing inverted
                message("Getting cached data.")
                return(m)
        }
        data <- x$get()                     ## Solve if there does not exist on
        m <- solve(data, ...)
        x$setsolve(m)
        m
}