## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ##  Initialize the inverse property
        i <- NULL
        
        ## Method to set the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## Method to get the matrix
        get <- function() x
        
        ## Method to set and get the inverse of the matrix
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        
        ## Return a list of the methods
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        
        ## Return the inverse if its already set
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## Get and set the matrix from the object
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
