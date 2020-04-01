## Assignment: Caching the Inverse of a Matrix


## Write the following functions:
## 1. makeCacheMatrix: This function creates a special "matrix" object that can
##    cache its inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix"
##    returned by makeCacheMatrix above. If the inverse has already been
##    calculated (and the matrix has not changed), then the cacheSolve should
##    retrieve the inverse from the cache.




## Put comments here that give an overall description of what your
## functions do

## This first function is as described above, a function which will be used
## to create a matrix object and cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    Inverse.Matrix <- NULL
    
    Set.Matrix <- function(y = matrix()) {
        x <- y
        Inverse.Matrix <- NULL
    }
    
    Get.Matrix <- function() x
    
    Set.Inverse <- function(Inverse) Inverse.Matrix <<- Inverse
    Get.Inverse <- function() Inverse.Matrix
    
    list(Set.Matrix = Set.Matrix, Get.Matrix = Get.Matrix,
         Set.Inverse = Set.Inverse,
         Get.Inverse = Get.Inverse)
}


## This function will find the inverse of the matrix created in makeCacheMatrix
## If the matrix is already known in the cache it will skip the calculation and
## get it from the cache.

cacheSolve <- function(x, ...) {
    
        Inverse.Matrix <- x$Get.Inverse()
        
        if(!is.null(Inverse.Matrix)) {
            message("Getting Cached Data")
            return(Inverse.Matrix)
        }
        
        Matrix.Var <- x$Get.Matrix()
        Inverse.Matrix <- solve(Matrix.Var)
        x$Set.Inverse(Inverse.Matrix)
        Inverse.Matrix
}

## Testing with an invertible matrix
m <- matrix(c(0,2,3,4,4,2,1,0,8),3,3)
test <- makeCacheMatrix(m)
test$Get.Matrix()
test$Get.Inverse()
cacheSolve(test)
test$Get.Inverse()
cacheSolve(test)

















