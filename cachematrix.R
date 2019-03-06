# Followed the example that was provided in the Assignment page
#First function acutally returns 4 functions, and it also stores the Matrix and its inverse when the second function is called
# Second function simply returns the Inverse , ( from the cache if it was there before, or else calculates if it's a new one)

makeCacheMatrix <- function(x = matrix()) {
        mInv <- NULL
        set <- function(y) {
                x <<- y
                mInv <<- NULL
        }
        get <- function() x
        setInv <- function(inVAns) mInv <<- inVAns
        getInv <- function() mInv
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}

cacheSolve <- function(x, ...) {
        mInv <- x$getInv()
        if(!is.null(mInv)) {
                message("getting cached data")
                return(mInv)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
