## This function creates a list that includes 4 member functions:
## set, get, setInv and getInv. It uses <<- assignment operator to
## assign a value to an object in an environment that is different
## from the current environment. 

makeCacheMatrix <- function(x = matrix()) {
  
  xinv <- NULL		# Initializing variable for matrix inversion
  
  # set function is used to set a matrix to object created by 
  # makeCacheMatrix function
  
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  
  # This function returns the input matrix
  
  get <- function() {
    x
  }
  
  # This function set the inversed matrix
  
  setInv <- function(inv) {
    xinv <<- inv
  }
  
  # This function returned the inversed matrix
  
  getInv <- function() {
    xinv
  }
  
  # The below list is returned by makeCacheMatrix
  # and makeCacheMatrix object can be used similar to Class object
  # for example, makeCacheMatrix$set
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function uses solve() function to calculate inverse matrix and save the result in cache,
## any successive request for inverse matrix calculation for same matrix data will be retrieved
## from cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Here 'x' is the output of makeCacheMatrix
  
  inv = x$getInv()
  
  # If the inverse has been calculated already
  
  if (!is.null(inv)) {
    
    # get the inverse calculation from cache and skip computation
    
    message("getting cache data ...")
    return(inv)
  }
  
  # If not inverse matrix has been calculated then, calculate as below
  
  matrix_data <- x$get()
  
  # Use solve function to calculate matrix inversion
  inv <- solve (matrix_data)
  
  # set the value of the inverse matrix in the cache using the setInv function.
  
  x$setInv(inv)
  
  # Return the inverse matrix calculation
  message("getting calculated data ...")
  return(inv)
}