#===========================File Descriptor ===================================
## This file contains functions for creating a cached matrix and it's inverse. 
#It also gives you accessor and modifier functions to change them.
# Since taking the inverse of a matrix can be an expensive operation in terms 
#of time and cpu resources, it can help to calculate it once and then store it 
#for future use. 
#=============================================================================

## makeCacheMatrix is a function that creates a special cached matrix object 
#It returns a list of functions that allow you to access and 
#modify the matrix and it's inverse. If you set a new matrix, it will null
#the previous inverse that was cached.
makeCacheMatrix <- function(x = matrix()) {
	matrixInverse <- NULL
    #define the functions used for acessing and modifying the matrices
	setMatrix <- function(newMatrix){
			x <<- newMatrix # update the x matrix
			matrixInverse <<- NULL #adding a new matrix, nulls the previous inverse		
	}#end of function setMatrix	
	getMatrix <- function() x
	setInverse <- function(newInverse) matrixInverse <<- newInverse
	getInverse <- function() matrixInverse
	#now return a list of functions that can access and modify the cached data
	list(setMatrix = setMatrix, getMatrix = getMatrix,
	setInverse = setInverse, getInverse = getInverse)	
}#end of function makeCacheMatrix
#=============================================================================

## cacheSolve is a function that calculates the inverse of the special matrix
#object created by the makeCacheMatrix function, ONLY IF the inverse is not
#already cached.
#NOTE:For now, it is assumed that,the matrix is invertible (square matrix)
cacheSolve <- function(x, ...) {
        inverseAnswer <- x$getInverse()
		if(!is.null(inverseAnswer)){
			message(" inverse was cached, getting cached data")
			return(inverseAnswer)
		}
		originalMatrix <- x$getMatrix()
		inverseAnswer <- solve(originalMatrix,...)
		x$setInverse(inverseAnswer)
		inverseAnswer
}#end of function cacheSolve


