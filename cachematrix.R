## These functions allow you to return the inverse of a matrix that has previously been
## calculated from a cached value; if not previously calculated it will calculate the
## inverse and cache the result for subsequent calls to save on computations.

## This function creates a special "matrix", which is really a list containing functions
#  to get and set the matrix data, and, get and set the inverse of the data.

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL              # Variable to store inverse
    set <- function(y) {
        x <<- y              # Cache the original matrix
        Inv <<- NULL         # Set the matrix inverse to NULL
    }    
    get <- function() x      # Return original matrix
    setinverse <- function(Inverse) Inv <<- Inverse # Set the inverse matrix given by passed in "Inverse"
    getinverse <- function() Inv # Return the cached inverse
    
    # Return a name list with named elements to each fuction
    
    list( set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)

}


## The following function calculates the inverse of the special "matrix" created with the 
#  above function. However, it first checks to see if the inverse has already been
#  calculated. If so, it gets the inverse from the cache and skips the computation. 
#  Otherwise, it calculates the inverse of the matrix and sets the value of the inverse in the 
#  cache via the setinverse function

cacheSolve <- function(x, ...) {
        
    # Get the inverse and see if already cached if so return it
    
    Inv <- x$getinverse() 
    if(!is.null(Inv)) {
        message("getting cached data")
        return(Inv)
    }
    
    # Otherwise get the original matrix data and calculate the inverse using solve
    
    data <- x$get()
    Inv <- solve(data, ...) 
    x$setinverse(Inv) # Cache the inverse for future reference/use
    Inv # Return the inverse matrix
}
