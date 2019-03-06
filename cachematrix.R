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
