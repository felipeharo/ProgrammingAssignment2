## The following functions are meant to be used in case the 
## inverse of a matrix has to be calculated many times
## The functions calculate the inverse of a particular matrix
## and if the same inverse has to be calculated again it uses 
## a cached value stored from the previous calculation


## this function returns a list of functions to get a matrix value 
## and to set/get the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {# This is a constructor function
  m<-NULL # m is initialized with the NULL value when makeCacheMatrix is created, meaning that there is no cached value
  get<-function()x # this function returns the value of the matrix x
  setsolve<-function(inverse) m<<-inverse # this function assigns to m(parent value) the inverse matrix of x. The value is cached
  getsolve<-function() m #this function returns the cached value of the inverse matrix. m will be NULL if no inverse matrix has been calculated
  list(get = get, #a list with the 3 functions needed is returned
       setsolve = setsolve,
       getsolve = getsolve)
}


## This functions returns the inverse of the matrix x
## It checks if the inverse of the matrix has been cached
## If it is cached, it returns the cached value, otherwise it is calculated and cached

cacheSolve <- function(x, ...) {#this function returns the inverse of the matrix x
  m<-x$getsolve() #the cached value of the inverse is assigned to m (it is NULL if it doesn't exist)
  if(!is.null(m)) #it is checked if the cached value exists
    return(m) #if the cached value exists, it is returned
  #if the cached value doesn't exist, the function continues
  data <- x$get() #the matrix to be evaluated is assigned to data
  m <- solve(data, ...) #the inverse of the matrix is calculated and assigned to m
  x$setsolve(m) #the inversed of the matrix is cached
  m #the result is returned
}
