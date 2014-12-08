## Put comments here that give an overall description of what your
## functions do

#' Generic object that can cache the result of an operation on a matrix.
#' 
#' \code{makeCacheMatrix} creates a matrix object that provides means to cache a
#' matrix and the result of an operation on it.
#' 
#' @param x The matrix that should be operated on in the cacheXxx-function.
#' @return This function closure as list.
#' @examples
#' cachedMatrix <- makeCacheMatrix(matrix(rnorm(1:9), 3, 3))
makeCacheMatrix <- function(x = matrix()) {
        r <- NULL
        f <- NULL
        set <- function(y) {
                x <<- y
                r <<- NULL
                f <<- NULL
        }
        get <- function() x
        setresult <- function(result, fun) {
                r <<- result
                f <<- fun
        }
        getresult <- function() r
        getfun <- function() f
        list(set = set, get = get,
             setresult = setresult,
             getresult = getresult,
             getfun = getfun)
}

.verifyResult <- function(x, m) {
        # Test if cached result in 'x'. If not, return NULL.
        # Stop execution if result in 'x' not from function 'm',
        # otherwise return cached result in 'x'.
        r <- x$getresult()
        if (!is.null(r)) {
                f <- x$getfun()
                if (f != m)
                        stop("Cached result for ", f, "(), not for ", m, "().")
                message("getting cached data")
        }
        r
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- "solve"
        r <- .verifyResult(x, m)
        if(is.null(r)) {
                data <- x$get()
                r <- solve(data, ...)
                x$setresult(r, m)   
        }
        r
}


cacheCrossprod <- function(x, ...) {
        ## Return a matrix that is the cross-product of 'x'
        m <- "crossprod"
        r <- .verifyResult(x, m)
        if(is.null(r)) {
                data <- x$get()
                r <- crossprod(data, ...)
                x$setresult(r, m)   
        }
        r
}


cacheColMeans <- function(x, ...) {
        ## Return a vector that is the colMeans of 'x'
        m <- "colMeans"
        r <- .verifyResult(x, m)
        if(is.null(r)) {
                data <- x$get()
                r <- colMeans(data, ...)
                x$setresult(r, m)   
        }
        r
}
