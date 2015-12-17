## Below are two function that take a squere (2X2) matrix and returns its inverse values.

###### to test use: a <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2)) #######
# 1st function: makeCacheMatrix takes a given 2 X 2 mmatrix and also provides a list containig function to: 
# 1. set matrix 
# 2. get matrix
# 3. set matrix inverse
# 4. get matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
    
  }
  get <- function() x # returns the original matrix. test with a$get()
  setinverse <- function(solve) inv <<- solve # do not test!!
  getinverse <- function() inv  # if tested before cashSolve function will return "null" else will return matrix inverse. test with a$getinverse() 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
# 2nd function: cashSolve, takes the original values of the matrix and store it to inv. 
# if inv is not missing it will return the cashed matrix inverse.
# else it will compute the matrix inverse using solve() function, cash the result in inv and returns matrix inverse.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()  # checks if inv is null if not return cached values
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()  # in case values are not in cache
  inv <- solve(data) # compute using solve()
  x$setinverse(inv)  # store in cache
  inv                # return vmatrix inverse
} 
