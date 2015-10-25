
## function CACHE
## function to compute the inverse of a square matrix using a cache

## Last updated: October 24, 2015
## Author: Vanessa Fens


# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(myMatrix = matrix()) {
        myCachedInverse <- NULL # this parameter is stored in this functions environment 
        setNonInvertedMatrix <- function(newMatrix) {
                myMatrix <<- newMatrix # the setNonInvertedMatrix function is called from a different environment so use <<- in stead of <-
                myCachedInverse  <<- NULL 
        }
        getNonInvertedMatrix <- function() myMatrix # return myMatrix
        setInverse <- function(inverseFromCacheSolve) myCachedInverse  <<- inverseFromCacheSolve # save the inverse matrix in this function's environment
        getInverse <- function() myCachedInverse # return the cached inverse 
        
        # the output of this function is a list of function
        list(setNonInvertedMatrix = setNonInvertedMatrix, 
             getNonInvertedMatrix = getNonInvertedMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
      
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(listOfFunctions, ...) {
        
        tempvar <- listOfFunctions$getInverse()  # = myCachedInverse, either NULL or cached inverse
        if(!is.null(tempvar)) {
                message("getting cached data")
                return(tempvar) # returns cached inverse
                # functie ends here
        }
       # no cached inverse yet, so calculate it here
       # first retrieve original matrix
        data <- listOfFunctions$getNonInvertedMatrix()
        # calculate inverse
        tempvar <- solve(data, ...)
        # insert the cached matrix into the cache!
        listOfFunctions$setInverse(tempvar)
        tempvar
}

