## This file contains functions that makes use of a caching technique
## in getting the inverse of a matrix

## makeCacheMatrix function makes a "special" matrix that can set/get
## a matrix and its corresponding inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}


## cacheSolve function computes the inverse of a "special" matrix if
## it still doesn't exist and returns it if it already exists

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) inv
	data <- x$get()
	inv <- solve(data)
	x$setinverse(inv)
	inv
}
