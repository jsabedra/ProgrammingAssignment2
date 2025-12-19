
## This function is a typeof constructor that:
##     1: sets the value of the matrix
##     2: gets the value of the matrix
##     3: sets the inverse of the matrix
##     4. gets the inverse of the matrix

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


## This function calculates the inverse of a matrix.  However, 
## if the inverse of that matrix has already been solved, thus function
## gives the previously stored inverse and skips the computation

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
        ## Return a matrix that is the inverse of 'x'
}
