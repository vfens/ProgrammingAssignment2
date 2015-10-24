
## function CACHE
## function to compute the inverse of a square matrix using a cache

## Last updated: October 24, 2015
## Author: Vanessa Fens


# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(myMatrix = matrix()) {
        myCachedInverse <- NULL
        setNonInvertedMatrix <- function(newMatrix) {
                myMatrix <<- newMatrix
                myCachedInverse  <<- NULL
        }
        getNonInvertedMatrix <- function() myMatrix
        setInverse <- function(inverseFromCacheSolve) myCachedInverse  <<- inverseFromCacheSolve
        getInverse <- function() myCachedInverse 
        
        # this is the output
        list(setNonInvertedMatrix = setNonInvertedMatrix, 
             getNonInvertedMatrix = getNonInvertedMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
      
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(listOfFunctions, ...) {
        ## Return the inverse matrix 
  
       tempvar <- listOfFunctions$getInverse()  # = myCachedInverse, either NULL or cached inverse
        if(!is.null(tempvar)) {
                message("getting cached data")
                return(tempvar) # returns cached inverse
                # functie ends here
        }
       # no cached inverse yet, so calculate here
       # first retrieve original matrix
        data <- listOfFunctions$getNonInvertedMatrix()
        # calculate inverse
        tempvar <- solve(data, ...)
        # insert the cached matrix into the cache!
        listOfFunctions$setInverse(tempvar)
        tempvar
}

