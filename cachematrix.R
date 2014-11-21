## The Function pairs allows the Creation of a presistent matrix,
## And Caching it's inverse for further use.


## Creates a presistent matrix, Which and retains the inverse matrix.
## Used for Caching (does not calculate the inverse, just retains it)
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Takes as input a list created by makeCacheMatrix, 
## and returns the cached inverse if exsits.
## Otherwise calculates the inverse, addes it to the cachedMatrix, and returns it.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        
}
