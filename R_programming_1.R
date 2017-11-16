#makeCacheMatrix: This function creates a special "matrix" object 
#that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  
  get <- function() x
  
  setiInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  
  
  list(
    set = set, 
    get = get,
    setInverse = setInverse,
    getInverse = getInverse)	
}


#cacheSolve: This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been 
#calculated (and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("Getting cached matrix")
    return(inv)
  }
  
  Math<- x$get()
  inv <- solve(Math, ...)
  
  
  x$setInverse(inv)
  inv    
}