## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        i <- matrix()
        set <- function(y) {
                x <<- y
                i <<- matrix()
        }
        get <- function() x #stores the original matrix data in get
        
        setSolve <- function(solve) i <<- solve    #caches the inverse to parent environmen.                                    Here, solve, needs to be in lower case, since R is case sensitive to calling                                        functions including variables
        
        getSolve <- function() i
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)

}


## Write a short comment describing this function

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

#mt <- matrix(data = c(4,7,2,6), nrow = 2, ncol=2, byrow = TRUE)           # an invertible matrix 

#mc <- makeCacheMatrix(mt)     # converts matrix to cacheable list data

#cacheSolve(mc) #At first, solves for the inverse of matrix, mt. Subsequently, it gets inverse from cache

