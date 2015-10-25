
  ## makeCacheMatrix does what the name suggests - creates a matrix and returns a list of created functions.

makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize cache
    cache <- NULL

    set <- function(y) {
      x <<- y
      cache <<- NULL
    }
    
  # Get the matrix
    get <- function() x
  
  # Invert and save the matrix
    setinverse <- function(inverse) cache <<- inverse
  
  # Get the inverted matrix
    getinverse <- function() cache
  
  
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# Below function looks for the inverse of a matrix. If nothing is found, the inverse is created and stored in cache.

cacheSolve <- function(x, ...) {
  
  # Find if the inverted matrix exist
    cache <- x$getinverse()
    
  # If the inverted matrix does exist, return it
    if(!is.null(cache)) {
      return(cache)
    }
  
  # If not, get the matrix
    matrixForInv <- x$get()
  
  # Invert it
    cache <- solve(matrixForInv)
  
  # Save in cache
    x$setinverse(cache)
  
  # And return
    cache
}


