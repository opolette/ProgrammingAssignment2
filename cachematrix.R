## makeCacheMatrix: creates a "matrix object" that caches a matrix and its inverse
## cacheSolve: computes the inverse of a matrix using the makeCacheMatrix function. 
## If the matrix is already cached in a makeCacheMatrix object, then simply retrieves the value.

## Sample usage
## > source("cachematrix.R")
## > m <- matrix(c(1,3,5,2,2,6,3,4,0), nrow=3, ncol=3)
## > mObj <- makeCacheMatrix(m)
## > mObj$getInverse() ## Inverse not set: will return NULL
## NULL
## > mObj$setInverse()
## > mObj$getInverse()
## [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
##
## Now using cacheSolve function
## mObj2 <- makeCacheMatrix(m)
## First call: inverse not cached - it is caculated
## > cacheSolve(mObj2)
## [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
## First call: inverse has been cached by first call - it is retrieved - print for demo purpose here only
## > cacheSolve(mObj2)
## [1] "Return cached inverse"
## [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1


## makeCacheMatrix is provide an invertible matrix as an input.
## It creates with this matrix a special "matrix object" that exposes sets of closures (returned as a list)
## get: gets the matrix
## setInverse: calculates the inverse of the matrix and caches it
## getInverse: gets the inverse of the matrix
##

makeCacheMatrix <- function(x = matrix()) {
  if (det(x) == 0) {
    stop("Non inversible matrix")
   }
  m <- x
  mInverse <- NULL
  get <- function() {
    m
  }
  setInverse <- function() {
    mInverse <<- solve(m)
  }
  getInverse <- function() {
    mInverse
  }
  list(get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve: uses a "matrix object" created with the makeCacheMatrix function as an input
## returns the inverse of the "matrix object". If the inverse has already been calculated,
## cacheSolve retrieves it from the cache. Else it calculates it and caches it, using the 
## makeCacheMatrix.setInverse closure

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if (!is.null((m))) {
    print("Return cached inverse")
    return(m)
  }
  x$setInverse()
  m <- x$getInverse()
  return(m)
}
