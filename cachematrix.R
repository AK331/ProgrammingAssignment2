## Computes the inverse of a matrix and caches the results

## Calculates the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinv <- function(inv) m <<- inv
	getinv <- function() m
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Computes the inverse of the matrix in makeCacheMatrix 
## or retrieves the cached values

cacheSolve <- function(x, ...) {
      m <- x$getinv()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinv(m)
	m
}

