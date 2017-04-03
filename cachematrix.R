## The makeCacheMatrix and cacheSovle function that I created are used to calculate the inverse of a matrix.
## Function structures are similar to the ones of caluclating means in the example.

## makeCacheMatrix take input x as you put in the brackets and created variables x,rev,set,get,setrev,getrev

makeCacheMatrix <- function(x = matrix()) {
  rev <- NULL
  set <- function(y){
    x <<- y
    rev <<- NULL
  }
  get <- function() x
  setrev <- function(revfun) rev <<- revfun
  getrev <- function() rev
  list(set=set, get=get,
       setrev=setrev,
       getrev=getrev)
}


## cacheSolve function will calculate the inverse of matrix you put in from previous function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  rev <- x$getrev()
  if(!is.null(rev)) {
    message('getting cached data')
    return(rev)
  }
  data <- x$get()
  rev <- solve(data,...)
  x$setrev(rev)
  rev
}

