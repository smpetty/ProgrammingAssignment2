## These two functions calculate the inverse of the matrix using 
## the solve function in R. The functions assume the matrix is 
##always invertible, a warning will be printed if the input matrix
##is not invertible.

## The makeCacheMatrix takes an nxn matrix and caches it
## cacheSolve uses the input makeCacheMatrix function to calculate
## the inverse of the matrix. It will look for the inverse in getsolve
## variable in the cached matrix. If it is NULL then it will use the setsolve 
## function within makeCacheMatrix to create the inverse.

makeCacheMatrix <- function(matrx = matrix()) {
    ## Return a matrix 'matrx'
    ## the input should be an nxn matrix 
    ## e.g., matrx <- makeCacheMatrix(matrix(matrix(rnorm(25),5,5)))
    ##
    ##First check that matrix, matrx, is invertible 
    if (dim(matrx)[1] != dim(matrx)[2]) {
        print("WARNING! This matrix is invertible, and cannot be used for cacheSolve function.")
    }else{print("Matrix is invertible with dimensions: ")
                print(paste(dim(matrx)[1],dim(matrx)[2],sep='x') )}
    vm <- NULL
    ##Setting the value of the matrix to cache, where y is a dummy variable
    setmatrx <- function(y) {
        matrx <<- y
        vm <<- NULL
    }
#getting the value of the matrix to cache if no value is set prior
    getmatrx <- function() matrx
#setting the inverse of the matrix using solve
    setsolve <- function(solve) vm <<- solve
#getting the inverse of the matrix using solve
    getsolve <- function() vm
    list(setmatrx = setmatrx, getmatrx = getmatrx,
         setsolve = setsolve,
         getsolve = getsolve)
}   



## Write a short comment describing this function

cacheSolve <- function(matrx, ...) {
    ## Return a matrix that is the inverse of 'matrx'
    ## the input should be the matrix function created from makeCacheMatrix 
    ## e.g., matrx <- makeCacheMatrix(matrix(matrix(rnorm(25),5,5)))
    ##if the inverse is already cached in the matrx input, then
    ## it will pull the matrx$getsolve() values
        vm <- matrx$getsolve()
        if(!is.null(vm)) {
            message("getting cached data")
            return(vm)
        }
        data <- matrx$getmatrx()
        vm <- solve(data, ...)
        matrx$setsolve(vm)
        vm
}