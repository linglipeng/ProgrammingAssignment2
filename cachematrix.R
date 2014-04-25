## Put comments here that give an overall description of what your
## functions do
## 	makeCacheMatrix - Function to create a "special cache matrix" that takes
##		an matrix input, and returns a list of 4 functions:
##		set(), get(), setInverse(), and getInverse()
##	cacheSolve - Function to calculate the inverse of a matrix or from cache. 
##		It takes an input of the "special cache matrix" from 
##		makeCacheMatrix() and returns the matrix inverse 
##		either by calculating or from cache

## Write a short comment describing this function
## Function makeCacheMatrix takes input of a matrix, and returns
##       a list of 4 functions set(), get(), setInverse() and getInverse()

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inv_val) inv <<- inv_val
	getInverse <- function() inv
	list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
## Function cacheSolve takes input of a "special cache matrix" from makeCacheMatrix,
##	and returns the inverse of the matrix either from calculation or cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInverse()
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setInverse(inv)
	inv
	
}
