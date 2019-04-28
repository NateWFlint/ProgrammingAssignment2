## makeCacheMatrix is a function that takes a matrix as its only argument. The environment
## of the function includes the values x (the matrix itself), and inv, which is used to
## store the inverse of x. This is initially set to null. The additional functions
## get, setinv, and getinv are also defined within the environment of the function
## makeCacheMatrix. get() retrieves the stored value of x. getinv() retrieves the stored
## value of the inverse of x. set() assigns a new matrix in place of x and resets the 
## stored inverse to NULL.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)

}

## cacheSolve is a function that can be called on objects of the type makeCacheMatrix. It
## retrieves the stored value for the inverse of the the matrix x, or if if there is no
## stored value, calculates the inverse first and then returns the solution.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("Getting cached matrix.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

## Demonstration of solution...
# INPUT and INPUT2 are both arbitrary 4x4 square matrices. 
INPUT <- matrix(sample(c(1:100), size=16), nrow = 4, ncol=4)
INPUT2 <- matrix(sample(c(101:200), size=16), nrow=4, ncol=4)
# MRX is a makeCacheMatrix object 
MRX <- makeCacheMatrix(INPUT)
# Running MRX's get function returns the initial input
MRX$get()
# Running MRX's set function changes the stored matrix to a different matrix. Re-running
# get() then returns the new value.
MRX$set(INPUT2)
MRX$get()
# Running MRX's getinv function initially returns NULL. While a matrix has been stored, 
# its inverse has not yet been calculated and stored.
MRX$getinv()
# Running cacheSolve on MRX returns the inverse of the inverse of the cached matrix and
# also caches the inverse 
cacheSolve(MRX)
# Running cacheSolve a second time without changing the cached matrix returns the same
# result, but with the console message that the result is getting retrieved from the cache
cacheSolve(MRX)
