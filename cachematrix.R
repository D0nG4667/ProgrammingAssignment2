## This R code contains a pair of functions that is able to cache the Inverse of a Matrix,
## a potentially timeconsuming computation 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- matrix()
        set <- function(y) {
                x <<- y
                i <<- matrix()
        }
        get <- function() x #stores the original matrix data in get
        
# Here, solve, needs to be in lower case, R is case sensitive to calling functions including variables.
        setSolve <- function(solve) i <<- solve    #caches the inverse to parent environment.           
        
        getSolve <- function() i
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getSolve()
        if(!(identical(i,(matrix())))) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setSolve(i)
        i
}


## Remove comment tag, #, from below (mt,mc,cacheSolve(mc)) only, to test the above functions

#mt <- matrix(data = c(4,7,2,6), nrow = 2, ncol=2, byrow = TRUE)          # A square invertible matrix 

#mc <- makeCacheMatrix(mt)     # converts matrix to cacheable list data

#cacheSolve(mc) #At first, solves for the inverse of matrix, mt. Subsequently, it gets inverse from cache

