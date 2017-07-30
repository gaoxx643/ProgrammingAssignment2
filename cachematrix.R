## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## make the matrix as input 
makeCacheMatrix <- function(x = matrix()) {
		inv_Matrix <- NULL   
## define the value of the matrix		
		setMatrix <- function(y) {  
			x << y            
			inv_Matrix << NULL  
		}
## get the value of the matrix		
		getMatrix <- function() x
## set the value of the invertible matrix		
		setInverse <- function(inverse) inv_Matrix << inverse
## get the value of the invertible matrix
		getInverse <- function() inv_Matrix
		list(setMatrix = setMatrix, getMatrix = getMatrix, 
			 setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
## obtain the value of the invertible matrix from makeCacheMatrix function 
		invMatrix <- x$getInverse()
## if inverse in not NULL
		if(!is.null(invMatrix)) {
## type message			
			message("Getting Cached Invertible Matrix")
## return invertible matrix
			return(invMatrix)
		}     
 ## if value of matrix is NULL
 ## get the original matrix data		
 		MatrixData <- x$getMatrix()
 ## inverse the matrix through SOLVE
 		invMatrix <- solve(MatrixData, ...)
 ## set the invertible matrix
 		x$setInverse(invMatrix)
 ## return invertible matrix
 		return(invMatrix)	    
 ## Return a matrix that is the inverse of 'x'
}
