##
## 'MakeCacheMatrix': This function creates a special "matrix" object
##                    that can cache its inverse.

## `cacheSolve`: This function computes the inverse of the special
##               "matrix" returned by `makeCacheMatrix` above. If the inverse has
##               already been calculated (and the matrix has not changed), then
##               `cacheSolve` should retrieve the inverse from the cache.

## 

makeCacheMatrix <- function(x = matrix()) {
        cacheMatrix <- NULL     # Define the cache Matrix 'cacheMatrix'
                                # Assign the value 'NULL' to the 'cacheMatrix'
        
        setMatrix <- function(y) {  # Define the method named 'setMatrix'
                x <<- y
                cacheMatrix <<- NULL
        }
        
        getMatrix <- function() x  # Define the method named 'getMatrix'
                                   # Return the matrix 'x'
        
        setCache <- function(inverse) cacheMatrix <<- inverse # define the method named 'setCache'
        
        getCache <- function() cacheMatrix # Define the method named 'getCache'
                                           # That will return the cached inverse of 'x'
        
        list(setMatrix = setMatrix, getMatrix = getMatrix, # List the names of all methods 
             setCache = setCache,
             getCache = getCache)

}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cacheMatrix <- x$getCache() # Check the content of cached matrix
        
        if(!is.null(cacheMatrix)) { # If the inverse has already been calculated
                message("Getting the inversed matrix from cache!!")
                return(cacheMatrix)
        } 
        
        defMatrix <- x$getMatrix() # Assign Matrix
        
        cacheMatrix <- solve(defMatrix, ...) # Calculate the inverse and assign to cacheMatrix
        
        x$setCache(cacheMatrix) # Updateing cache
        
        return(cacheMatrix) # Return the inversed matrix
        
}
