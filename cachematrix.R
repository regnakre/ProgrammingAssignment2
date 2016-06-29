# Matrix inversion is usually a costly computation and there may be some benefit  to caching the inverse of a matrix rather than compute it repeatedly. The following two functions are used to cache the inverse of a matrix.


# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    x_i  <- NULL
    set <- function(y) {
        x <<- y
        x_i <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inv) x_i <<- inv
    getInverse <- function() x_i
    list(set = set, get = get, 
         setInverse = setInverse, getInverse = getInverse)
}

# The following function returns the inverse of the matrix. 
#It first checks if the inverse has already been computed and exists in the cache. If so, it gets the result and skips the computation. 
#Otherwise, it computes the inverse using solve() function and sets the value in the cache using setinverse().
cacheSolve <- function(x, ...) {
        x_i <- x$getInverse()
        if(!is.null(x_i)){
            message("getting cached data")
            return(x_i)
        }
        data <- x$get()
        x_i <- solve(data, ...)
        x$setInverse(x_i)
        x_i
}
?matrix

#creating a 2x2 matrix to test the code
x = matrix(seq(0, 1, length.out = 4), ncol=2, nrow=2)
m <- makeCacheMatrix(x)
m$get()
# display the matrix
#          [,1]      [,2]
#[1,] 0.0000000 0.6666667
#[2,] 0.3333333 1.0000000

# first call, no value stored in the cache
cacheSolve(m)
#     [,1] [,2]
#[1,] -4.5    3
#[2,]  1.5    0

# second call, value should be fetched from the cache
cacheSolve(m)
#getting cached data
#     [,1] [,2]
#[1,] -4.5    3
#[2,]  1.5    0






