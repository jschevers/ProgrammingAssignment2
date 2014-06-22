## two functions to compute the inverse (using solve) of a matrix and store it in cache
## next time inverse must be calculated retreive it from cache instead of re-calc it


## creates 'special' matrix and stores it in cache
makeCacheMatrix <- function(x = matrix()) {
  ## Return a 'special' matrix
    s <- NULL                             # reset inverse to NULL-object
    
    set <-function (y){                   # store in cache    
      x <<- y
      s <<- NULL
    }
    get <- function() x                   # read from cache
    
    setsolve <- function(inv) s <<- inv   # store in cache   
    getsolve <- function() s              # read from cache

## return list with set-and getfunctions    
    list(set=set,get=get,
         setsolve=setsolve,getsolve=getsolve)
}


## retreive inverse of matrix from cache, or 
## calculate inverse of matrix (using "solve") and store it in cache 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
     s <- x$getsolve()                   # get the inverse from cache
     if (!is.null(s)){                   # checks if inverse stored in cache      
         message("getting cached data")  # there's really is a inverse in cache
         return(s)                       # return the inverse I got from cache
    }
  # nothing found in cache, now calculate the inverse
     data <- x$get()
     s <- solve(data, ...)               # calculate inverse, dots for "solve" arguments
     x$setsolve(s)                       # store the inverse in cache
     s                                   # return the inverse of x
}
