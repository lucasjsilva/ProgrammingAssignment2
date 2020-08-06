## This program contains two functions that will cache the inverse of a matrix

## First of all, makeCacheMatrix() creates an object 
##that can cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL # first I set the inverse to NULL so afterwards I can place a value 
      set <- function(y){
            x <<- y # defines x to y
            i <<- NULL  # reset the inverse to NULL
      }
      get <- function() x #returns the matrix
      setinverse <- function(solve) i <<- solve #sets the inverse to solve
      getinverse <- function() i #returns the inverse
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## The next function will compute the inverse matrix form the function above
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)){
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
        ## Return a matrix that is the inverse of 'x'
}
