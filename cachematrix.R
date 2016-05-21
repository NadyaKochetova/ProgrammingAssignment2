
## create a list that contain functions to set and get a matrix  as ## well as get and set inverse matrix  

makeCacheMatrix <- function(x = matrix()) {
  #this function returns a list that contains functions
  m<-NULL
  # define a list with functions
  list(get=function() x
       , 
       set=function(y) {
         x <<- y
         m <<- NULL
       },
       setInverse=function(solve) m<<-solve
       , 
       getInverse=function() m
      )
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'. If available, cached value is returned 
 
  # get inverse matrix
  m<-x$getInverse()
  # check if inverse matrix is available 
  if(!is.null(m)) {
    message("getting cached data")
    return(m) #  return value and exit the function
  }
  
  # get matrix
  matrix<-x$get()
  # inverse matrix and push to cache
  m <- solve(matrix)
  x$setInverse(m)
  
  # return inverse matrix  
  m
  
}