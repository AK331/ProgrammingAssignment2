## Computes the inverse of a matrix and caches the results

## Creates a special "matrix" which is just a list containing a function to
## get and set the maxtrix and to get and set the inverse 

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
## If the inverse has already been cached, it retrieves that inverse
## Else it calculates the inverse and sets it as a cached inverse

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

