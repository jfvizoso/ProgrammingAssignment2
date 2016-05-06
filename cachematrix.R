## This functions are used to cache the inverse of a matrix

## Create a matrix with special properties to hold the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Get the inverse of the cache. The inverse is got from cache if available. 
## If the inverse is not cached, it's calculated and then cached for next use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m  
}

## Example of use
## create a matrix
## m <- makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1)) )
## see the matrix content
## m$get()
## calculate the inverse
## cacheSolve(m)
## getting the inverser again, this time from cache
## cacheSolve(m)



