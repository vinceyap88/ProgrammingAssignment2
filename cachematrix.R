## Matrix inversion is usually a costly computation which can be relieved by 
## caching the inverse of a matrix using the following functions.
## Example: 
##	 # generate the random square, non-singular matrix
##         testmatrix<-matrix(runif(1,10), 100, 100)
##       # generate cache object with this matrix
##	   cachematrix<-makeCacheMatrix(testmatrix)
##       # calculate the inverse of the matrix and cache the result 
##	   invmatrix<-cacheSolve(cachematrix)

## This function works like a class, which contains 
## setter and getter functions for the matrix, and as well as inverse matrix.
## It contains the internal variable to keep the result of inversion.

makeCacheMatrix <- function(x = matrix()) {
	## here to keep the result of inversion
	invx <- NULL 

        ## set a matrix to object
	set <- function(y) {
        	x <<- y
		# initialize it to null
		invx <<- NULL
        }
	## return the input matrix
	get <- function() x

	## set the inversed matrix
	setInv <- function(inv) invx <<- inv
	## return the inversed matrix
	getInv <- function() invx 
	
	## return a list that contains functions 
	## which can be used by used by makeCacheMatrix object
	## x <- makeCacheMatrix(testmatrix)
				# change matrix
	list(set = set,		# x$set(anothermatrix) 
				# get the matrix 
 	     get = get,		# x$get
				# set the inversed matrix 
             setInv = setInv,   # x$setInv
				# get the inversed matrix
	     getInv = getInv	# x$getInv
	    )
}


## This function is to calculate the inversed matrix 
## or to retrieve from the cache

cacheSolve <- function(x, ...) {
	## get the inversed matrix from object x
	m <- x$getInv()
	
	## check if the inversion result is there
	if (!is.null(m)) {
	  message("Found cached inversed matrix.")
	  # return the result
	  return(m)
        } 

	## if not found, get the matrix object
	data <- x$get()

	## calculate it
	m <- solve(data)

	## keep the result into cache
	x$setInv(m)
	
        ## Return a matrix that is the inverse of 'x'
	m
}
