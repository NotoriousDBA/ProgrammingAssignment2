## Matrix inversion is a costly computation. This pair of functions attempts to
## mitigate this fact by allowing the inverse of a matrix to be cached once it's
## calculated.

## This function creates a "special" matrix that allows the inverse of the matrix
## to be cached. It takes a matrix as its only argument, and returns a list of the
## getter and setter functions used to get/set the matrix itself, and the matrix's
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # Initialize the inverse of the matrix to NULL.

  # Set the value of the matrix, and blank out the inverse.
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  # Return the value of the matrix.
  get <- function() x

  # Save the inverse of the matrix.
  setinverse <- function(inverse) i <<- inverse

  # Return the inverse of the matrix.
  getinverse <- function() i

  # Return the list of functions used to interact with this "special" matrix.
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of the "special" matrix x. It returns the
## cached copy of the inverse if it's available, otherwise it calculates the
## inverse from scratch.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'.

  # Get the cached inverse of the matrix, if it exists.
  i <- x$getinverse()

  # Do we have a cached inverse?
  if (!is.null(i)) {
    # Yep. All good.
    message("getting cached data")
  } else {
    # We don't have the inverse of the matrix yet. Let's fix that.

    data <- x$get()     # Get the matrix's data.
    i <- solve(data, ...)       # And call solve() to calculate its inverse.
    x$setinverse(i)     # Cache the inverse.
  }

  # Return the inverse of the matrix.
  i
}
