## Pair of functions that cache the inverse of a matrix

## Function that holds values of the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  # Inverse property
  .inverse <- NULL
  
  set <- function( matrix ) {
    x <<- matrix
    setInverse(NULL)
  }
  
  get <- function() x
  
  setInverse <- function(inverse) .inverse <<- inverse
  
  getInverse <- function() .inverse
  
  list(
    set = set, 
    get = get, 
    setInverse = setInverse, 
    getInverse = getInverse
  )
}


## Function that retrieves the cached value or 
## solves the inverse of the matrix and caches it
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  .inverse <- x$getInverse()
  
  if( !is.null(.inverse) ) {
    message("getting cached data")
    return( .inverse )
  }
  
  .matrix <- x$get()
  
  .inverse <- solve( .matrix )
  
  x$setInverse( .inverse )
  
  .inverse
}
