## Pair of functions that cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
# construct a special “matrix” object that can cache its inverse.
  ivrs <- NULL
  set <- function(y) {
        x <<- y
        ivrs <<- NULL
  }
  get <- function() x
  setivrs <- function(inverse) ivrs <<- inverse
  getivrs <- function() ivrs
  list(set=set, get=get, setivrs=setivrs, getivrs=getivrs)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ivrs <- x$getivrs()
  if(!is.null(ivrs)) {
          message("getting cached data.")
          return(ivrs)
  }
  data <- x$get()
  ivrs <- solve(data,...)
  x$setivrs(ivrs)
  return(ivrs)
}



# to test, 
#> r=rnorm(100)
#> mat= matrix(r,nrow=10, ncol=10)
#> makeCacheMatrix(mat)

