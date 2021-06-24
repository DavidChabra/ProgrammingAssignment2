## These Functions cache a matrix as a list, 
## which can contain its inverse matrix.


## Stores initial matrix and defines
## additional functions that let you adjust its contents. 

makeCacheMatrix <- function(x = matrix()) {
    I <- NULL
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) I <<- inverse
    getinverse <- function() I
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## Finds the inverse of a matrix cached by makeCacheMatrix().
## It does this by first attempting to recover a priviously stored
## value then, by solving the maxtix using the solve() function.

cacheSolve <- function(x, ...) {
    I <- x$getinverse()
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setinverse(I)
    I
}