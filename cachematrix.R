(### This code consists of two function to calculate and cache the inverse of a matrix ,
### this inverse is stored and will be deleted once the special matrix is deleted or changed
### this code has two functions, the first creates a special "matrix" object that can
### cache its inverse to be available when ever it is needed
###  The other function computes the inverse of the special "matrix" returned by makeCacheMatrix function
###  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
### should retrieve the inverse from the cache
###  functions do

## This function can be used to creat a special matrix that can be conjegated with the 
##  inverse of the matrix. changing the matrix results in erasing the old cached inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function returns the inverse of the special matrix. it calculates the inverse only if
## it is not calculated before then cach the inverse

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

