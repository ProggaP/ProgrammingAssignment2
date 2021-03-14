## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
  inv <- NULL                 ##initialize inv as NULL                  
  set <- function(y) {                    
    x <<- y                             
    inv <<- NULL                      
  }
  get <- function() x                     
  
  setInverse <- function(inverse) inv <<- inverse   ##set the inverse of the matrix
  getInverse <- function() inv                      
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)   
  
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("Return cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv        ## returns a matrix that is a inverse of x
}
