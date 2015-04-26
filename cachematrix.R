## Purpose of assignment - supposed to create two functions that can cache the inverse of a matrix

## Function: create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     
     ## 1) set the value of the matrix
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     
     ## 2) get the value of the matrix
     get <- function() x
     
     ## 3) set the value of the inverse
     setinverse <- function(solve) m <<- solve
     
     ## 4) get the value of the inverse
     getinverse <- function() m
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Function: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     ## check if inverse has already been calculated and if matrix has not changed
     if(!is.null(m) && identical(mat2$get(),x$get())){
          message("getting cached data")
          return(m)
     }
     # computes inverse of the matrix otherwise
     data <- x$get()
     m <-solve(data, ...)
     x$setinverse(m)
     m
     

}


