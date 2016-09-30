## The function (makeCacheMatrix) creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  ## Defining Cache
  n <- NULL
  set <- function(y) {
    x <<- y    ## Assigning the input matrix to the variable in the parent environment
    n <<- NULL  ## Reinitializing n in the parent environment to null
  }
  get <- function() x    ## Returning the matrix.
  setInverse <- function(inverse) n <<- inverse   ## Set the cache n equal to the inverse of matrix x
  getInverse <- function() n      ## Returning the cached inverse of x
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)
}


## The function (cacheSolve) computes the inverse of the matrix created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  n <- x$getInverse()
  if (!is.null(n)) {
    message("get data cache")
    return(n)
  }
  mdata <- x$get()
  n <- solve(mdata, ...)
  x$setInverse(n)
  n
}
