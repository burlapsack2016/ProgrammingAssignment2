## Put comments here that give an overall description of what your
## functions do

## This will cache the the inverse data from this created matrix


makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y){
			x <<- y
			m <<- NULL
		}
		get <- function()x
		setinverse <- function(inv) m <<- inv
		getinverse <- function() m
		list(set = set, get = get,
			 setinverse = setinverse,
			 getinverse = getinverse)
}


## This will compute the previously cached data

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
        	message("getting cached data")
        	return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
