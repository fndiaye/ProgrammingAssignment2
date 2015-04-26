## These functions allow to calculate the inverse of a matrix,
## using cached objects in order to re-calculate already calculated data
## The code is divided into 2 functions
## The 1st one allows to cache calculated object
## The 2nd one retrieve the inverse matrix if cached
## Otherwise, calculate the inverse of the matrix


## makeCacheMatrix
## 'x' is a matrix for which the inverse will be calculated
## Is composed by 4 sub-functions: setMatrix(y), getMatrix(), setMatrixInverse(matrice), getMatrixInverse()


## Returns a list of the 4 functions above

makeCacheMatrix <- function(x = matrix()) {
      matrixInverse <- NULL
      
      ## This sub-function assignes a value to x
      ##'y': the matrix to assigned to x
      setMatrix <- function(y) {
        x <<- y
        matrixInverse <<- NULL
      }
      
      ## This sub-function retrieves the value of x
      getMatrix <- function() x
      
      ## This sub-function assignes a value to matrixInverse for caching
      ##'matrice': a inverse matrix
      setMatrixInverse <- function(matrice) matrixInverse <<- matrice
    
      ## This sub-function retrieves the value of the cached matrix inverse
      getMatrixInverse <- function() matrixInverse
    
      ## Returns a list of these 4 functions
      list(setM = setMatrix, getM = getMatrix, setMI = setMatrixInverse, getMI = getMatrixInverse)

}


## cacheSolve
## 'x': the list returned from the function 'makeCacheMatrix'
## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        
      ## Gets the matrix inverse
      mi <- x$getMI()
          
      ## if the inverse matrix is cached, retrieves it from the cache
      if(!is.null(mi)) {
        message("getting cached data")
        return(mi)
      }
      
      ## if not cached, gets the matrix, calculates its inverse and caches the result
      data <- x$getM()
      mi <- solve(data, ...)
      x$setMI(mi)
      mi
     
      
}
