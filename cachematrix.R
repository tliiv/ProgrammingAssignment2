## Created by Toomas Liiv for assignement 2 in the course 
## "R Programming" in the Data Science Specialization on Coursera

## Two functions for calculating the inverse of a matrix and storing it
## in a cache, to speed up repeated calls. 


## Creates object for storing a matrice and its inverse in get/set style

makeCacheMatrix <- function(x = matrix()) {
          # initializes inverse
          inv <- NULL
          
          # sets matrix and inverse outside of scope
          set <- function(y) {
            x <<- y
            inv <<- NULL
          }
          # retrieves matrix
          get <- function() x
          
          # caches the matrix outside of scope
          setinverse <- function(inverse) inv <<- inverse
          # retrieves the matrix
          getinverse <- function() inv
            
          # returns list with the four functions
          list(set = set, 
               get = get,
               setinverse = setinverse,
               getinverse = getinverse)
}


## solves a matrix (calculates inverse) by, if it exists, retrieving 
## it from cache, or if not, calculating it and saving it to cache

cacheSolve <- function(x, ...) {
          # retrieves inverse from cache
          inv <- x$getinverse()
          # if inverse exists, returns it
          if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
          }
          
          # if not in cache:
          
          # retrives matrix from object
          data <- x$get()
          # solves the matrix
          inv <- solve(data, ...)
          # saves it to cache
          x$setinverse(inv)
          # returns inverse 
          inv
}
