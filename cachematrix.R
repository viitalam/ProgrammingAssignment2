
## Creates a list of four functions used to create a matrix. 
## I sets a new value to a matrix and overwrites previous inverse as NULL
## II gets the current value
## III  sets value for inverse
## IV gets the current inverse


makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  ## I
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  ## II
  get <- function() x 
  ## III
  setinv <- function(inv) I <<- inv
  ## IV
  getinv <- function() I
  
  list(set = set, get = get, getinv = getinv, setinv = setinv)
}


## Uses the matrix structure created before, looks for computed value of inverse
## and if there is none, calculates it.

cacheSolve <- function(x, ...) {
  ## See if inverse has been calculated
  I <- x$getinv()
  if (!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  ## Get matrix, calculate inverse and put the new value in.
  matrix <- x$get()
  I <- solve(matrix)
  x$setinv(I)
  I
  
}
