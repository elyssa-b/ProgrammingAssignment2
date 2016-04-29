## These functions cache an input matrix, search, and retrive the inverse of the 
## matrix if it has been cached
## If the inverse of the matrix has not been cached, it will be computed

## This function caches the input matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function (y){
		x <<- y
		m <<- NULL
	}
	get <- function () x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function () m
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function searches for the cached inverse of the input matrix and returns it if 
## available
## If the inverse of the matrix is not cached it will be computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if (!is.null(m)){
	message("getting cached data")
	return(m)
	}
	data <- x$get()
	m <- solve(data,...)
	x$setinverse(m)
	m
}
