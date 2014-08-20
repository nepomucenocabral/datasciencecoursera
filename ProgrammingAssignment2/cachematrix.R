## makeCacheMatrix will compute and store the inverse of a 
## given matrix in a given R object

## cacheSolve will check if the inversed matrix is cached
## If the result is not cached, cacheSolve will cache it
## If the result is cached, it will print the inversed matrix

## Compute and stores the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
        m = NULL
        set = function(y) {
            x <<- y
            m <<- NULL
        }
        get = function() x
        setinverse = function(solve) m <<- solve
        getinverse = function() m
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Checks if the inversed matrix is cached
## If it is, the inversed matrix is printed
## If it is not, cacheSolve will cache the data

cacheSolve <- function(x, ...) {
        m = x$getinverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data = x$get()
        m = solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
