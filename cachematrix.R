## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <-function(y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      Sinvers <- function(inverse) m <<- inverse 
      Ginvers <- function() m
      list(set=set, get=get, Sinvers=Sinvers, Ginvers=Ginvers)
      
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$Ginvers()
      if(!is.null(m)){
            message("getting cached ")
            return(m)
      }
      res <- x$get()
      m <- solve(res, ...)
      x$Sinvers(m)
      m
}

