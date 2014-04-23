##Takes in a matrix and caches it, the other function retrieves a cached inverse
##or solves and caches the inverse

## Creates a list of functions that can geta and set Matrix data
##as well as get and set the Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
    ## create variable i
    i <- NULL
    ## create function set which will set the matrix, and null i, so that there
    ## no data cached
    set <- function(y) {
    x <<- y
    i <<- NULL
    }
    ## retrieves the value of the matrix passed in
    get <- function() x
    ## function to set the inverse matrix
    setInverse <- function(inverse) i <<- inverse
    ## retrives the inversed matrix
    getInverse <- function() i
    ##returns a list of all the functions
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)

}


## solves for the inverse of the matrix and caches it, if it has not been solved
## already, if solved it just retrieves the cached answer

cacheSolve <- function(x) {
  ##retrieves data cache
  i <- x$getInverse()
  ## checks for a cached inverse
  if(!is.null(i)) {
    ## message indicating data was cached
    message("getting cached data")
    return(i)
  }
  ##retrieves the matrix to inverse
  data <- x$get()
  ## get the inverse
  i <- solve(data)
  ##set the inverse
  x$setInverse(i)
  ##print the inverse
  i
}
