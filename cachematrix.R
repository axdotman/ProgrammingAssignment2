## R Programming Coursera
## Assigment 2
## 12-13-14
## 
## Create special matrix object that allows caching of its inverse.
## Add a function to calculate the cache and store results into CacheMatrix

## Create a matrix object with features to cache the inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function(y) {
        x   <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
        setmean = setmean,
        getmean = getmean)
}

## Check if an inverse matrix has been cached. If not, 
## calculate and cache the inverse. Return inverse matrix.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
