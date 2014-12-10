#' The functions in this file work together to cache the result of operations on
#' matrices.
#' 
#' Each cacheXxx function in this file takes a cached matrix and additional 
#' parameters to calculate the result of a particular operation on a matrix. 
#' Once the result is calculated, subsequent calls on the cacheXxx functions 
#' will return the cached result.
#' 
#' A cached matrix can be obtained by calling the function 
#' \code{\link{makeCacheMatrix}} with the matrix to operate on as parameter. A
#' cached matrix object stores the results of diverse operations internally in a
#' list.


#' Generic object that can cache the results of operations on a matrix.
#' 
#' \code{makeCacheMatrix} creates a matrix object that provides means to cache a
#' matrix and the results of operations on it.
#' 
#' @param x The matrix that should be operated on in the cacheXxx-function.
#' @return This function closure as list, a cached matrix.
#' @examples
#' cachedMatrix <- makeCacheMatrix(matrix(rnorm(1:9), 3, 3))
#' cacheSolve(cachedMatrix)
#' cacheCrossprod(cachedMatrix)
#' cacheColMeans(cachedMatrix)
makeCacheMatrix <- function(x = matrix()) {
        results <- list()
        set <- function(y) {
                x <<- y
                results <- list()
        }
        get <- function() x
        setresult <- function(result, fun) results[[fun]] <<- result
        getresult <- function(fun) results[[fun]]
        list(set = set, get = get,
             setresult = setresult,
             getresult = getresult)
}


#' Generic helper function to get and set the result of an operation on a cached
#' matrix.
#' 
#' \code{.cacheX} is an internal helper function for the cacheXxx functions on
#' matrices.
#' 
#' Internal function that does the common routine:
#' 
#' - test if a result for operation \code{m} is cached in \code{x},
#' - if not:
#'      - get the original matrix from \code{x};
#'      - call on the given function \code{fun}, with the original matrix
#'              and other parameters in \code{...};
#'      - store the result for operation \code{m} in \code{x}.
#' - else:
#'      - display a message that we return the cached result.
#' - return the result.
#' 
#' @seealso \link{makeCacheMatrix}
#' @param x A cached matrix.
#' @param m The name of the wrapped operation.
#' @param fun A wrapper function around the operation that should yield the
#'   result.
#' @param ... Other parameters for the wrapped operation.
#' @return The result of the operation in \code{fun} on the cached matrix.
.cacheX <- function(x, m, fun, ...)
{
        r <- x$getresult(m)
        if(is.null(r)) {
                data <- x$get()
                r <- fun(data, ...)
                x$setresult(r, m) 
        } else {
                message("getting cached data for function ", m)
        }
        r
}


#' Inverse a matrix and cache the result.
#' 
#' \code{cacheSolve} does exactly what \code{\link[base]{solve}} does on a 
#' single invertible matrix. In addition it stores the result on the given
#' cached matrix.
#' 
#' @seealso \link[base]{solve}
#' @seealso \link{makeCacheMatrix}
#' @param x A cached matrix.
#' @param ... Additional parameters for the function solve.
#' @return The inverse of the matrix in the given cached matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        .cacheX(x, "solve", fun <- function(data, ...) {
                solve(data, ...)
        }, ...)      
}

#' Calculate the cross product of a matrix and store the result.
#' 
cacheCrossprod <- function(x, ...) {
        ## Return a matrix that is the cross-product of 'x'
        .cacheX(x, "crossprod", fun <- function(data, ...) {
                crossprod(data, ...)
        }, ...) 
}

#' Calculate the column means on a matrix and store the result.
#' 
cacheColMeans <- function(x, ...) {
        ## Return a vector that is the colMeans of 'x'
        .cacheX(x, "colMeans", fun <- function(data, ...) {
                colMeans(data, ...)
        }, ...) 
}
