##================================================================
## to write a pair of functions that cache the inverse of a matrix.
## for the peer assessment assignment of “R Programming” on coursera.
## created by: Li Gongzuo
## Email: alan.gz.li@msn.cn
##================================================================


## Create makeCacheMatri function：This function creates a special "matrix" object 
## that can cache its inverse

makeCacheMatri <- function(x = matrix()) {              
        if (det(x)==0) {        ##the matrix supplied isn't invertible, return Error
                message("Error:the matrix supplied should be invertible!")
        } else {                ##the matrix supplied is invertible,make cache matrix
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
             getsolve = getsolve)
        }
} 


## Create cacheSolve function：This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

