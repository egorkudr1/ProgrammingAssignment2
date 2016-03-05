## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a list with 4 methods.
# method sets take matrix into a list.
# method gets give matrix from a list.
# method setinv takes iverse matrix into a list.
# method getinv gives inverse matrix from a list.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set, get=get, setinv = setinv, getinv=getinv)
}


## Write a short comment describing this function
# This function takes x of special type, which was created with makeCacheMatrix, and return an inverse matrix.
# If this function calculated an inverse matrix for x before it won't calculate again it. 
# and  will make message "getting cached data". 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  inv <- solve(x$get(), ...)
  x$setinv(inv)
  inv
}
