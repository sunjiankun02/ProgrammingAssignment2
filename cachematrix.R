## The following two functions are used to create a special object
## that stores a matrix and caches its inverse.

## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse. 
## It creates a special "vector", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    ix <- NULL
    set <- function(y){
        x <<- y
        ix <<- NULL
    }
    get <- function() x
    setinv <- function(inv) ix <<- inv
    getinv <- function() ix
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieve the inverse from the cache.
## Otherwise, it calculates the inverse of the matrix 
## and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    ix <- x$getinv()
    if(!is.null(ix)){
        message("getting cached inverse")
        return(ix)
    }
    mtx <- x$get()
    ix <- solve(mtx,...)
    x$setinv(ix)
    ix
}
