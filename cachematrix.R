## matrix wrapper/convinience functions, to manage inverse matrix calculation and caching


##test data, used to perform simple validation
testMatrix <- matrix(c(1, 30, 500, 7, 9, 11, 13, 15, 17), nrow = 3, ncol = 3)
inv <- solve(testMatrix)


## makeCacheMatrix: Create "wrapper" set of functions to manage matrix data
## and to calculate inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invrt <- NULL
  set <- function (y) {
    x <<-y
    invrt <<-NULL
  }
  get <- function() x
  setcache <- function(solve) invrt <<- solve
  getcache <- function () invrt
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}


##calculate inverse of a matrix stored within object returned by makeCacheMatrix
##or return already calculated inverse matrix

cacheSolve <- function(x, ...) {
  currentInv<-x$getcache()
  if(!is.null(currentInv)){
    message("getting cached data")
    return(currentInv)
  }
  data<-x$get()
  currentInv <- solve(data, ...)
  x$setcache(currentInv)
  currentInv
}


