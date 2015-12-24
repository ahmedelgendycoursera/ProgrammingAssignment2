## These two functions using Lexical Scoping and environment assignment to cache expense objects. 


## This function returns a list of functions that are used to set and get a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL ## m with no value
  set <- function(y) {## to set the matrix to the matrix passed as argument and the inverse to null not in the execution environment of this function but in the enclosure of all of the inner functions.
    x <<- y
    m <<- NULL
  }
  get <- function() x ## to return the first x on the way up (will be the x set by set() or the argument passed in makeCacheMatrix().
  setInverse <- function(inverse) m <<- inverse ## to assign the inverse passed as argument to the m not in the execution environment of this function but in the enclosure of all of the inner functions.
  getInverse <- function() m ## to return the first m on the way up.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This functions tries to get the inverse of a matrix from cache and if the inverse is not stored in the cache for that particular matrix, the function computes the matrix inverse and caches the inverse.

cacheSolve <- function(l, ...) {
  m <- l$getInverse()##get the cached inverse.
  if(!is.null(m)) {##check if there is an inverse.
    message("getting cached data")
    return(m)## return the cached inverse.
  }
  data <- l$get()##get the cached matrix
  m <- solve(data)##compute the inverse
  l$setInverse(m)##cache the inverse
  m##return the inverse
}
