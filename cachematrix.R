#Assignment 2
#A function that creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL    ## Creating an empty variable m (will be used later to cache data)
  set <- function(y) {
    x <<- y    ##assigning y to the enviornment of x
    m <<- NULL ##assignging NULL to the enviornment of m
  }
  get <- function() {
    x          ##define get as a function that prints x
  }
  setsolve <- function(solve) {
    m <<- solve ##assignging solve to the enviornment of m
  }
  getsolve <- function() {
    m           ##define get solve as a function that prints m
  }
  list(set = set, get = get, ####create a final list that prints all of my values and there enviornments
       setsolve = setsolve,
       getsolve = getsolve)
}



##This function computes the inverse of the special "matrix" returned by 
#makeCacheMatrix above. If the inverse has already been calculated (and the 
#matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getsolve() ##gather the getSolve() from makeCacheMatrix running on the matrix x
  if(!is.null(m)) { ##checking if m has a value if it does continue if it does not jump to else
    message("getting cached data") #status message indicating the data was cached
    return(m)       ##print the inverse 
  } else {  ##the data is not cached need to calculate the inverse and store in cache for future use
    data <- x$get() ##gather the get() from makeCacheMatrix running on the matrix x
    m <- solve(data, ...) ##calculate the inverse of the matrix
    x$setsolve(m)   ##define setsolve as m (listed in line above) from makeCacheMatrix
    m               ##print m the inverse matrix
  }
}

