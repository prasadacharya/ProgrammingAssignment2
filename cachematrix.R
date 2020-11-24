#1# This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

# Initializing the inverse 
  i <- NULL

# Creating Method to set the matrix
  set <- function(y){
         x <<- y
         i <<- NULL
  }

# Creating Method to get the matrix
  get <- function() x

# Creating Method to set the inverse of the matrix
  setInverse <- function(inverse) i <<- inverse

# Creating Method to get the inverse of the matrix
  getInverse <- function() i

# Returning a list of the methods
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

#2# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated(and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
# Returning a matrix that is the inverse of 'x'
  i <- x$getInverse()

# Returning the inverse if its already set
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }

# getting the matrix
  matrix <- x$get()

# Calculating the inverse
  i <- solve(matrix, ...)

# Setting the inverse
  x$setInverse(i)

# Returning the matrix
  i      
}