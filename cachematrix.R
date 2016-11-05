## We calculate once the inverse of a matrix and cache its values. So we don't 
## need to redo this costly computation.


## We make a list with the necessary functions to set the matrix and get the mean.

makeCacheMatrix <- function(X = matrix()) {
        i <- NULL      
        set <- function(y) {
                X <<- y
                i <<- NULL
        }
        get <- function() X
        setsol <- function(solve) i <<- solve
        getsol <- function() i
        list(set = set, get = get,
             setsol = setsol,
             getsol = getsol)
}

## Using the elements of the returned list we calculate the inverse. If it is already 
## calculated we return the solution.

cacheSolve <- function(X, ...) {
        i <- X$getsol()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- X$get()
        i <- solve(data, ...)
        X$setsol(i)
        i
}