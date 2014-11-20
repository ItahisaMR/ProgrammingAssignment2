## This pair of functions returns an inverse matrix from a catched object.

## First, we create a special matrix which list a function to:
## 1. set and get the value of the matrix
## 2. set and get the value of the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs <<- inverse
  getinverse <- function() invs
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This calculate the inverse by the matrix created before.First it have to check
## if the inverse have already been calculated, and if not it computes via 
## setinverse.

cacheSolve <- function(x, ...) {
  invs <- x$getinverse()
  if(!is.null(invs)) {
    message("getting cached data.")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data)
  x$setinverse(invs)
  invs
}
