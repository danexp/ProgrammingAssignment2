
##Those function does the invertion of a matrix, and stores the result in memory
##for a next call, If so, it gets the invertion from the cache and skips the computation.

##This function receives a matrix as argument and inverts the matrix, 
##and stores it in memory

makeCacheMatrix <- function(x = matrix()) {        
        s <- NULL 
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) s <<- solve
        getInverse <- function() s
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##The following function does the inversion of the special "matrix" created 
##with the above function. However, it first checks to see if the invertion 
##has already been calculated. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInverse()
        if(!is.null(s)) {
                message("getting cached matrix result")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setInverse(s)
        s
}
