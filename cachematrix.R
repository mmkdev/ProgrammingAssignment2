## The function allows you to save both a matrix and inverse of the matrix 
## set - Lets you set the Matrix
## get - Lets you get the Matrix
## setInverse - Lets you set the inverse of a Matrix
## getInverse - Lets you get the inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
	
	#Clears variable if already set by the caller
	m <- NULL
	#Lets you set the Matrix
   set <- function(y) {
   		x <<- y
   		m <<- NULL
   }
   #Lets you get the Matrix
   get <- function() x
   
   #Lets you set the inverse of a Matrix
   setInverse <- function(inverse) m <<- inverse
   
   #Lets you get the invese Matrix you saved
   getInverse <- function() m
   list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)


}


## This function checks if your makeCacheMatrix object has a cachedMatrix
## if it does it returns the CachedMatrix
## otherwise it solves the inverse of the matrix and saves it into the cachedMatrix object

cacheSolve <- function(x, ...) {
	#checks if makeCacheMatrix passed in has Inverse set
	inverse <- x$getInverse()
	
	#If it does it returns the cached inverse 
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
   }
	
	#if it doesn't it gets the matrix
	data <- x$get()
	#finds the Inverse
	m <- solve(data, ...)
	#caches with Inverse
	x$setInverse(m)
	# prints out the Inverse
	m
}
