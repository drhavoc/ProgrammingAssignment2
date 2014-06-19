## This is Programming Assignment 2 for the R Programming Coursera course.
## See README.md

## The goal is to provide a data structure that represents a matrix with a cached inverse in it,
## and a function to retrieve (or calculate, if not cached) that inverse from the data structure.

## We use the function "makeCacheMatrix" to create the data structure, and the function "cacheSolve"
## to retrieve (or calculate, if not cached) the inverse.

makeCacheMatrix <- function(mat = matrix()) 
{
	# Initialize the cached inverse to NULL (not calculated)
	cachedInverse <- NULL

	# Function to set a new value to the matrix
	set <- function (newMatrix)
	{
		# Replace the matrix
		mat <<- newMatrix
		
		# Since we changed the matrix, the cached inverse is no longer valid
		cachedInverse <<- NULL 
	}
	
	# Function to retrieve the matrix
	get <- function () { mat }
	
	# Function to replace the cached inverse (used by "cacheSolve", not by the user)
	setCachedInverse <- function (inv ) { cachedInverse <<- inv }

	# Function to retrieve the cached inverse (used by "cacheSolve", not by the user)
	getCachedInverse <- function ( ) { cachedInverse }
	
	list ( set = set, get = get, setCachedInverse = setCachedInverse, getCachedInverse = getCachedInverse )
}

# This function returns the inverse (it "solves" the matrix) of the given "matrixWithCache" structure, which should
# have been created using "makeCacheMatrix"
cacheSolve <- function( matrixWithCache, ...) 
{
	# Check the cache
	cachedInverse <- matrixWithCache$getCachedInverse()
	
	# If not NULL, then it should be up to date, so we just return it
	if (!is.null(cachedInverse))
	{
		message("Using cached inverse")
		return(cachedInverse)
	}
	
	# No cache, so we have to calculate it
	message("Calculating and caching inverse")
	
	# Get the matrix
	mat <- matrixWithCache$get()
	
	# Invert it
	inverse <- solve(mat, ...)
	
	# Store it in the cache
	matrixWithCache$setCachedInverse(inverse)
	
	# And return it
	inverse
}

# Example usage. Notice the different messages the first time and the second time cacheSolve() is called.
# 
# > myMat = matrix ( c( 2, 0, 0, 0, 2, 0, 0, 0, 2 ), ncol=3, nrow=3)
# 
# > myMat
# [,1] [,2] [,3]
# [1,]    2    0    0
# [2,]    0    2    0
# [3,]    0    0    2
# 
# > myCachedMat = makeCacheMatrix (myMat)
# 
# > cacheSolve( myCachedMat )
# Calculating and caching inverse
# [,1] [,2] [,3]
# [1,]  0.5  0.0  0.0
# [2,]  0.0  0.5  0.0
# [3,]  0.0  0.0  0.5
# 
# > cacheSolve( myCachedMat )
# Using cached inverse
# [,1] [,2] [,3]
# [1,]  0.5  0.0  0.0
# [2,]  0.0  0.5  0.0
# [3,]  0.0  0.0  0.5


