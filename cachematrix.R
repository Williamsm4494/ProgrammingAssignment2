
## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function
## makeCacheMatrix

## This function creates a special "matrix" object that can cache its inverse
## Create a special "matrix" (list) containing a function that will
  ## set & get the value of the matrix and 
  ## set & get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  } ## Set the value of the matrix
  get <- function() x ## Get the value of the matrix
  setinverse <- function(inverse) i <<- inverse ## Set the value of the inverse of the matrix
  getinverse <- function() i ## Get the value of the inverse of the matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) ## Return special "matrix" (list) 
}

## Write a short comment describing this function
## cacheSolve 

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated it gets the inverse from the cache and skips the computation
## If not it calculates the inverse of the data and sets the value of the inverse in the cache

cacheSolve <- function(x, ...) {
  i <- x$getinverse() 
  if(!is.null(i)) {
    message("getting cached data")
    return(i)   
  } ## If the inverse has already been calculated print the message "getting cached data" 
      ## and return the inverse without the any computation
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i) ## If the inverse has not been previously calculated then calculate the inverse
                    ## and set the value of the inverse in the cache
  i  ## Return a matrix that is the inverse of 'x'
}


## Test run of the function
matrixinitial <- matrix(1:4, 2, 2)
matrixinitial
matrixfinal <- makeCacheMatrix(matrixinitial)
cacheSolve(matrixfinal)
cacheSolve(matrixfinal)


