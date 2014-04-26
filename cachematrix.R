## First of all I'd like to apologize for mistakes
## because I'm not a native english speaker, but from Ukraine
## Thanks for understanding

## So there are 2 functions:makeCacheMatrix and cacheSolve
## In each function you can find step descriptions

## makeCacheMatrix() function sets, gets matrix and its inverse
## This function does not calculate, only sets and returns values

makeCacheMatrix <- function(x = matrix()) {  
  inv <- NULL  
  ##Function makeCacheMatrix() contains 4 functions:
  
  ## the first one, set(value), sets to the global variable 
  ## x the value of parameter value (matrix). more due to 
  ## setting new value of matrix, the inverse matrix (inv) must be NULL
  ## because we haven't calculate it
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## the second one, get() return the value of global variable 
  ## x (matrix). If we haven't call the funcion set(value) before, 
  ## return of the function get() wil be NULL
  get <- function() x
  
  ## the third function, setinverse(value), sets to the 
  ## global variable inv value of parameter value (matrix).
  ## In this function we mean it will be inverse matrix to matrix,
  ## which is defind in global variable x. We DON'T calculate it 
  ## here, just set
  setinverse <- function(p) inv <<- p

  ##the last function, getinverse(), returns the value of 
  ## global variable inv (inverse matrix). If we haven't calculated 
  ## and sets the value of inv through the function setinverse(value)
  ## then getinverse() returns NULL value
  
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve() function calculates inverse matrix or calls 
## its stored values using previous function
## Reminder: For this assignment, assume that the matrix supplied 
## is always invertible.

cacheSolve <- function(x, ...) {
  ## First, return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  ## Then checked, if there is a inverse matrix calculated 
  ## before. In case when inverse matrix for initial data 
  ## was calculated, function says it and return data 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Then initialize to value 'data' matrix calling the global
  ## variable x
  data <- x$get()
  
  ## Calculate inverse matrix using function solve() to data
  inv <- solve(data)
  
  ## And send a result to global variable
  x$setinverse(inv)
  inv
}

## How to use:
## first call 'variable'<-makeCacheMatrix('your matrix')
## 'variable'$get shows your matrix
## 'variable'$getinverse shows NULL
## cacheSolve('variable') shows you inverse matrix
