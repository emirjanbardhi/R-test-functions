## special matrix is created by the first function and used in the second function, caching the inverse and giving helping methods

## This function creates a matrix with attributes

makeCacheMatrix <- function(x = matrix()) {
  
  inv_ <- NULL
  
  set <- function(mat){ 
    x <<- mat
    inv_ <<- NULL
  }
  
  get <- function() x
  set_inverse <- function(inv) inv_ <<- inv
  get_inverse <- function() inv_
  
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


## creates a matrix using the above function; if the inverse of the matrix not yet calculated it's calculated and cached

cacheSolve <- function(x) {
  mat_inv <- x$get_inverse()
  
  if(!is.null(mat_inv)){
      message("getting inverse")
      return(mat_inv)
  }
  
  mat <- x$get()
  inv_ <- solve(mat)
  x$set_inverse(inv_)
  inv_
}
