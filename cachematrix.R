##==================================================================
## This R file contains two main functions:
##    makeCacheMatrix: which takes a matrix and can store it
##								inverse in cache for quick retrival.
##
##		cacheSolve:			Will return an inverted matrix.  If
##								the value is available in cache it
##								will return the cache value, other
##								wise, it will invert the matrix and
##								set it's value to be stored in cache.
##==================================================================

##------------------------------------------------------------------
## get and set functions for a matrix and its inverse
##------------------------------------------------------------------

makeCacheMatrix <- function(origMatrix = matrix()) {
	invMatrix = NULL

									#------------------------------------------
									# Set the orinal matrix and
									# initial the inverse matrix to NULL
									#------------------------------------------
	
	set = function(y) {
		origMatrix <<- y
		invMatrix <<- NULL
	}

									#------------------------------------------
									# Return the original matrix
									#------------------------------------------
	
	get = function() {
	 	origMatrix
	}
	
									#------------------------------------------
									# Set the value of the inverse matrix
									#------------------------------------------
	
	setInvMatrix = function(inverseIt) {
		invMatrix <<- inverseIt
	}
	
									#------------------------------------------
									# Return the original matrix
									#------------------------------------------
	
	getInvMatrix = function() {
		invMatrix
	}
	
									#------------------------------------------
									# Create list of available functions
									#------------------------------------------
	
	list(set = set, get = get,
		  setInvMatrix = setInvMatrix,
		  getInvMatrix = getInvMatrix)
	 
}


##------------------------------------------------------------------
## Return an inverted matrix.  If the inverted matrix isn't already
## cached, invert the original matrix and store its inverse in
## cache, then return the inverse.
## Let the user know if the inverse matrix is from cache.
##------------------------------------------------------------------

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	invMatrix = x$getInvMatrix()

									# If the inverse matrix exist return it
	
	if (!is.null(invMatrix)) {
		message("Inverted Matrix is from cache")
		return(invMatrix)
	}
	
									# Inverse the matrix
	
	normMatrix = x$get()
	invMatrix = solve(normMatrix)
	
									# Store the inverse matrix in cache
	
	x$setInvMatrix(invMatrix)
	
	return(invMatrix)

}
