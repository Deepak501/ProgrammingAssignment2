## Caching the Inverse of a Matrix

## Create a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve 
  getmatrix <- function() m
  
  list(set=set, 
       get=get, 
       setmatrix=setmatrix, 
       getmatrix=getmatrix)
  
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve function will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getmatrix()
  
  if (!is.null(m)){
    # get from the cache 
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  
  m <- solve(data, ...)
  
  x$setmatrix(m)
  
  return(m)
}

## Reference : Tips from Mentor Leonard Greski titled Demistifying makeVector()
