## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                     ## Initialize inv as NULL; will hold value of matrix inverse
        set <- function(y) {                            ## Define the set function to assign new
        x <<- y                                         ## Value of matrix in parent environment
    inv <<- NULL                                        ## If there is a new matrix, reset inv to NULL
  }
  get <- function() x                                   ## Define the get fucntion - returns value of the matrix argument
  setinverse <- function(inverse) inv <<- inverse       ## Assigns value of inv in parent environment
  getinverse <- function() inv                          ## Gets the value of inv where called
  list(set = set, get = get,                            ## Refer to the functions with the $ operator
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cacheinverse <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
}
