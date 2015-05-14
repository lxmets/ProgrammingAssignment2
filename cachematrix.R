## The following two functions are used to cache the inverse of a matrix 
## since computing the inverse of a matrix is computationally intensive.

## To test out the following two functions, load the following two functions and execute the following commands:
## > m <- matrix(c(4,2,7,6), nrow = 2)          ## create a 2x2 matrix called m
## > h <- makeCacheMatrix(m)                    ## h is the special matrix object made out of m using our function
## > p <- cacheSolve(h)                         ## calculate the inverse of h
## > q <- cacheSolve(h)                         ## calculate the inverse of h again - this time it will use the cached copy


## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        matInv <- NULL
        set <- function(y) {
                x <<- y
                matInv <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) matInv <<- solve
        getInverse <- function() matInv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## This function computes the inverse of the matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then this function retrieves the inverse from the cache.

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        matInv <- x$getInverse()
        if(!is.null(matInv)) {
                message("getting cached data")
                return(matInv)
        }
        mat <- x$get()
        matInv <- solve(mat)
        x$setInverse(matInv)
        matInv
}