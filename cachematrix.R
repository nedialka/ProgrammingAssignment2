# Assignment: Caching the Inverse of a Matrix
 
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
# retrieve the inverse from the cache.
cacheSolve <- function(x, ...){
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <-x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}

# Test run
# > new_matrix <- matrix(c(0, 3, 4, 6), nrow=2, ncol=2)
# > new_cache <- makeCacheMatrix(new_matrix)
# > cacheSolve(new_cache)
# [,1]      [,2]
# [1,] -0.50 0.3333333
# [2,]  0.25 0.0000000
# > cacheSolve(new_cache)
# getting cached data
# [,1]      [,2]
# [1,] -0.50 0.3333333
# [2,]  0.25 0.0000000
# > 