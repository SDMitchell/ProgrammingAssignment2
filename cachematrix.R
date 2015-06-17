##
## The gist of this code is to provide a class-like wrapper for a basic R matrix via scoping rules and a dispatch list.
##

## makeCacheMatrix - This is a factory function that creates a class-like wrapper around the passed-in argument x;
##    x is cached and the inverse of x (represented by m) is NULLed. Four operations (get(), set(), getInverse(),
##    setInverse()) are declared and have access to both x and its inverse via the scoping operator <<-. A list
##    containing each of the functions is returned so that they may be called by the owner of the wrapper.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setInverse <- function(inv) m <<- inv
	getInverse <- function() m
	list(set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

## cacheSolve - This method either computes the inverse of a wrapped matrix x (if it is the first time being computed) or
##    simply returns a previously cached result. If the result is being computed for the first time, it is stored via
##    a call to the setInverse() method so that any further calls will return the cached inverse rather than recomputing
##    the inverse from scratch. An intervening call to set() will NULL the cached inverse and will cause a recomputation to take place.
##    The call to set() is not demonstrated here.
##
##    The internal call to solve() may be affected by the caller via the extra arguments taken by the cacheSolve() function;
##    any extra arguments will be handed through to solve().
##
##    If the supplied matrix is singular and error will be produced by the appropriate library (LAPACK by default).

cacheSolve <- function(x, ...) {
	m <- x$getInverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setInverse(m)
	m
}
