## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
          inverse <- NULL
          set <- function (y){
          x <<- y
          inverse <<- NULL
  }
          get <- function () x
          set_inverse <- function (inverse) inverse <<- inverse
          get_inverse <- function () inverse 
          list (set = set, get = get, set_inverse = set_inverse, get_inverse=get_inverse )
}
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
        inverse = x$get_inverse()
  
        if (!is.null (inverse)){
            print ("getting cached data")
        }
        else {
            matrix_data = x$get()
            inverse = solve (matrix_data, ...)
            x$set_inverse(inverse)
        }
      inverse
}
