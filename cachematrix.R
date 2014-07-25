## creates "special" vector that contains a list with a function that: 
## sets and gets the values for a matrix $$ sets and gets matrix inverse  
makeCacheMatrix<- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x 
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# calculates the inverse of matrix but first checks to see 
# if the matrix inverse has already been calculated 
# if it has, it gets the inverse from cache instead of re-caculating
cacheSolve <- function(x, ...) {
        m <- x$getinverse() 
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)
        }
      
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}