rm(list=ls())
cat("\014")

## Cache matrix inversion

## makeCacheMatrix function returns a list of functions to:
## 1. Set matrix
## 2. Get matrix
## 3. Set inverse of matrix
## 4. Get inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Check if cache exists, else compute and return matrix inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

m <- matrix(rnorm(1000000),nrow=1000,ncol=1000)
mv <- makeCacheMatrix(m)
mv$set(m)

ptm <- proc.time()
cacheSolve(mv)
print(proc.time()-ptm)

ptm <- proc.time()
cacheSolve(mv)
print(proc.time()-ptm)
