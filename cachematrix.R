## For R language week 3 assignment 
## JL 2018-12-27

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solved) m <<- solved  #function put result into cache m
        getinverse <- function() m                   # read m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already 
##  been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinverse()
        if(!is.null(m)) {                 #test if cache exists      
                message("getting cached inverse data")
                return(m)
        }
        data <- x$get()     #if not cached, put the matrix into data
        m <- solve(data, ...)    # inverse the matrix                      
        x$setinverse(m)          # then put the inverse into cache
        m

}








#  test
#  x <- matrix(c(1,2,3,4),2,2)
#  y <- makeCacheMatrix(x)
# no cache:   cacheSolve(y) 
# run again:   cacheSolve(y)