## These functions calculate and store the inverse of a matrix.
## This can save time and processing power when the matrices in question are large.

## First function takes a square matrix and returns a list of functions.
## This list can be stored and the functions called using the $

makeCacheMatrix <- function(x = matrix()) {
  if (nrow(x) != ncol(x)) { ## Check for square matrix
    print("Must be a square matrix.")
  }
  cachemat <- NULL ## Set cached matrix to null
  set <- function(newmat) { ## Define "Set" function to set matrix value within makeCacheMatrix
    x <<- newmat
    cachemat <<- NULL
  }
  get <- function() x ## Define "Get" function which returns the matrix in makeCacheMatrix
  setinverse <- function(inv) cachemat <<- inv ## Define "setinverse" which sets the value of the cached matrix
  getinverse <- function() cachemat ## Define "getinverse" which returns the cached matrix
  list(set = set, get = get, ## Return all functions defined above in a list
       setinverse = setinverse,
       getinverse = getinverse)
}


## Second function takes the first function as its argument and checks for a cached inverse.
## If none is stored, it calculates and stores the inverse.

cacheSolve <- function(x, ...) {
  s <- x$getinverse() ## Pulls the "getinverse" function from previous result
  if(!is.null(s)) { ## If "getinverse" exists, returns inverse with a message that it is using cached data
    message("getting cached data")
    return(s)
  }
  data <- x$get() ## If inverse has not been chached, pulls "get" function from previous result
  s <- solve(data, ...) ## Solve function calculates inverse
  x$setinverse(s) ## Uses "setinverse" to cache inverse
  s ## Returns inverse
}
  
 
  
