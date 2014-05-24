## makeCacheMatrix and cacheSolve matrix provides caching for matrix inverse calculations. 
## When using cacheSolve instead of Solve for matrix inverse calculations, if the inverse 
## of the matrix has been already calculated, we are getting a cached result for faster computation.


## makeCacheMatrix initializes an object with the matrix in its argument - or with NULL if it is missing. 
## It provides 4 functions. set, get - for setting and getting the value of the matrix
## and setinverse and getinverse for getting the inverse of its matrix stored in its cache,
## or setinverse to set the value of its cache.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL # m is the cache. Setting the cache to null when creating the matrix the first time
  set <- function(y) { # set is used to set a new value for the matrix, and also clearing the cache for its inverse
    x <<- y 
    m <<- NULL
  }
  get <- function() x # get is used to return the value of our matrix
  setinverse <- function(inverse) m <<- inverse #when setting the inverse, put the inverse value into cache
  getinverse <- function() m #when getting the inverse, simple return the cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse) #returning the four functions for the object
  
}


## cacheSolve is used to calculate the inverse of the cached matrix. 
## The cached matrix has to be initialized by makeCacheMatrix first
## It returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
      
  
  # getting the inverse of x. If it is already calculated, we will get its value. 
  # Otherwise NULL, which means that we will have to calculate it for the first time
  inverse_solve <- x$getinverse() 
  if(!is.null(inverse_solve)) {
    message("getting cached data")
    return(inverse_solve)  # if we have something in the cache, simply return it
  }
  # if there is no hit in the cahce, we have to calculate the inverse of the matrix
  matrix_value <- x$get() #reading the value of the original matrix
  inverse_solve <- solve(matrix_value, ...) #calculating the inverse of our matrix
  x$setinverse(inverse_solve) #put the calculated inverse into the cache for next time
  inverse_solve # return the calculated inverse
  
}
