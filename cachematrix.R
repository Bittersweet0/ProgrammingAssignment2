
## function doinf caching the inverse of matrix
## Here we have couple of functions that create a special object that stored a matrix and cache its inverse


## Firs func create a matrix that chace ins inverse
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


## Second function computes the inverse of the matrix created by first function.
## this function retrieve the inverse from the cache , if inverse already been calcilated

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

