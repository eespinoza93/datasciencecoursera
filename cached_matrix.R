# Assignment week 3
## Applying the same theory to a matrix inverse----
### Cache the matrix provided----
makeCacheMatrix <- function(x = matrix()){
  
  i <- NULL
  
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inv) i <<- inv
  getinverse <- function() i 
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}

### Test the function 
makeCacheMatrix(matrix(c(2, 0, 0, 2), c(2, 2)))

### Get the inverse of the cached matrix----
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinverse(i)
  i
}

## Test the functions----
test_matrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), c(2, 2)))
cacheSolve(test_matrix)
