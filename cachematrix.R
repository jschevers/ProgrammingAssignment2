## Put comments here that give an overall description of what your
## functions do

## creates 'special' matrix and stores it in chache
makeCacheMatrix <- function(x = matrix()) {
 s <- NULL           #reset inverse
 set <-function (y){
   x <<- y
   s <<- NULL
 }
 get <- function() x
 setsolve <- function(inv) s <<- inv
 getsolve <-function() s
 list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


## calculates inverse of matrix x with function "solve"
## and stores it in cache 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <-x$getsolve() #get the inverse from cache
  if (!is.null(s)){   # there's really is a inverse in chache
    message("getting cached data")
    return(s)         #return tehe inverse I got from chache
  }
  # nothing found in chache, now calculate the inverse
  data <- x$get
  s <- solve(data,...) #calculate inverse, dots for "solve" arguments
  x$setsolve(s)       # put the inverse in cache
  s 
}
