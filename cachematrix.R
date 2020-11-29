## The purpose of the functions makeCacheMatrix and cacheSolve is to cache the inverse of a matrix.
## The purpose of caching the inverse of the matrix is to avoid computing it repeatedly. 

## Note the solve() function assumes the matrix is square

## The function makeCacheMatrix is a function that creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set <- function(y) {
    x<<- y
    i<<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}


## cacheSolve checks to see if the inverse of the "matrix" created above has already been calculated.
## If it has it gets the inverse from the cache and skips the computation. 
## If it hasn't, cacheSolve calculates the inverse via the setinverse function 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        }
