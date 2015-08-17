   ## These two functions cache the inverse of a matrix.

   ## The "makeCacheMatrix" Function below will create a 
   ##   special "matrix" object that can cache its inverse.
   ## This returns a list containing a function to:
   ##    1. set the value of the matrix
   ##    2. get the value of the matrix
   ##    3. set the value of the inverse
   ##    4. get the value of the inverse
   ## This list is the input of the "cacheSolve()" Function.

makeCacheMatrix <- function(x = matrix()) {
      ## Input variable 'x' is a square invertible matrix
    
    m <- NULL
    set <- function(y) {
            ## assigns values to 'x' and 'm' regardless of environment.
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, 
         getinverse = getinverse)
}


   ## The "cacheSolve" Function below computes the inverse of the
   ##   special "matrix" returned by "makeCacheMatrix" Function.

cacheSolve <- function(x, ...) {
      ## Input variable 'x' is output of "makeCacheMatrix" Function.
    
    m <- x$getinverse()
    
      ## If inverse was already calculated, the below gets value
      ##   from cache instead of computing again.
    if(!is.null(m)) {
        message("Whoa nelly! There's data already in your cache!")
        return(m)
    }
    
      ## This calculates inverse of original input matrix if not cached.
    data <- x$get()
    m <- solve(data, ...)
    
      ## This code sets value of inverse in the cache & returns it.
    x$setinverse(m)
    return(m)
}
