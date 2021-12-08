## Function which caches the inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
      x <<- y
      i <<- NULL
  }
  get <- function() x
  setInv <- function(inv) i <<- inv
  getInv <- function() i
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Calculates the inverse of a given matrix.
## If the Inverse is already calculated (and matrix didn't change)
## it returns the inverse from the cache

cacheSolve <- function(x, ...) {
        i <- x$getInv()
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setInv(i)
        i
}
