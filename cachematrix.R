## Function which caches the inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y){
      x <<- y
      s <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) s <<- solve
  getSolve <- function() m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Calculates the inverse of a given matrix.
## If the Inverse is already calculated (and matrix didn't change)
## it returns the inverse from the cache

cacheSolve <- function(x, ...) {
        s <- x$getSolve()
        if(!is.null(s)) {
            message("getting cached data")
            return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$setSolve(s)
        s
}
