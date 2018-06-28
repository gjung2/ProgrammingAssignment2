## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
ivrs = NULL
  set = function(y) {
        x <<- y
        ivrs <<- NULL
  }
  get = function() x
  setivrs = function(inverse) ivrs <<- inverse
  getivrs = function() ivrs
  list(set=set, get=get, setivrs=setivrs, getivrs=getivrs)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ivrs = x$getivrs()
  if(!is.null(ivrs)) {
          message("getting cached data.")
          return(ivrs)
  }
  data = x$get()
  ivrs = solve(data,...)
  x$setivrs(ivrs)
  return(ivrs)
}



