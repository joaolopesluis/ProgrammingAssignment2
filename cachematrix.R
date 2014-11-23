makeCacheMatrix <- function(x = matrix()) {
		## Creates a special "vector", which is a list containing a function to
		##
		##1.  set the matrix
		##2.  get the matrix
		##3.  set the inverse matrix
		##4.  get the inverse matrix
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

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x' only if it is stored in cache.
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