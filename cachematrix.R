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



 ## Return a matrix that is the inverse of 'x
 
 cacheSolve <- function(x, ...) {
   ## if can get the inverse, then quickly get it 
  m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
   ## if the inverse does not exist, then calculate and set new result 
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
