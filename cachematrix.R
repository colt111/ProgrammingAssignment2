## These functions cache a matrix and its inverse to avoid potentially
## expensive recalculation of inverse.

# Takes a matrix, returns list of functions to get and set matrix and inverse
# Note: to maintain integrity of cache, ONLY update matrix with set function
makeCacheMatrix <- function (m = matrix()) {
        inv <- NULL
        get <- function () m
        set <- function (new_m) {
                if (!identical(new_m, m)) {
                m <<- new_m
                inv <<- NULL
                }
        }
        getinv <- function () inv
        setinv <- function (new_inv) inv <<- new_inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

#Returns a matrix that is the inverse of 'm', from cache if available, else
# computed from underlying matrix, (updating cache before returning)
cacheSolve <- function(m, ...) {
        inv <- m$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- m$get()
        inv <- solve(data, ...)
        m$setinv(inv)
        inv
}

