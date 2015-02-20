## These functions will compute the inverse of a given matrix
## This is assuming the matrix is inversible 
## Once the inverse has been calculated it is stored in a cache
## If the inverse is requested again, the cached value is returned



## This function will create a special list containing functions
## These functions can be used to set and get the matrix used
## or to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function uses the special list created by 'makeCacheMatrix'
## It will check for a cached value of the inverse matrix and return it if found
## If no cached value is found, calculate, set and return the inverse matrix

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
