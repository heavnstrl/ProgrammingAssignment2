## creating two functions, first to create a temp matrix to pull from
## and second to solve for it

## takes a square matrix as argument and returns a list to use in cacheSolve 

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set = set, get = get, 
         setinv = setinv, getinv = getinv)

}



## takes a list from makeCacheMatrix and returns the inverse of the original 
## input

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  
  if(!is.null(i)) {
    message("getting cached data...")
    return(i)
  }
  m.data <- x$get()
  i <- solve(m.data)
  
  x$setinv(i)
  i
}
