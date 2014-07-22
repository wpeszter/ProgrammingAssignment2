## These two functions calculate the inverse of a matrix and
## cache the inverse matrix

## makeCacheMatrix makes a list containing 4 functions:
## set the vaule of the matrix, get the value of the matrix
## set the value of the inverse matrix, get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## the cacheSolve function first checks, whether the inverse matrix
## is already calculated. If yes, it gives back the stored inverse matrix
## if not, then it calculates the inverse matrix and gives back its value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m) ) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m

}
