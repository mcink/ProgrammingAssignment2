## Programmer: Marcin Karczmarczyk
## Date: 2015-07-26
## Function makeCacheMatrix stores and retrieves the value of matrix


## input: x - matrix we want to make calcualtions on
## output list - list of functions:
##               set - sets the value of the matrix
##               get - gets the matrix
##               sinv - sets the inverted matrix
##               ginv - gets the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
		set <- function (y) {
				x <<- y
				inv <<- NULL
		}
		get <- function() x
		sinv <- function (inverse) inv <<-inverse
		ginv <- function() inv
		list (set=set, get=get,
		      sinv = sinv,
		      ginv = ginv)
}


## Function cacheSolve solves for the inverse of the matrix and caches it.
## If the inverse of the matrix has been calculated previously it just
## retrieves it and displays. If it has not been calculated before, it
## calculates it and stores it.
## Assumption is that the matrix is always invertible
## input : x - matrix we want to check the inverse
## output: inv - the inverted matrix

cacheSolve <- function(x, ...) {
        inv <- x$ginv()
        if(!is.null(inv)) {
        	message ("Getting cached data")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$sinv(inv)
        inv
}
