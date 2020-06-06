## The below functions return inverse of a matrix and the inverse is cached 
## which helps to reduce repeated computations

## makeCacheMatrix function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invmatrix <- NULL
        set <- function(y){
                x <<- y
                y <<- NULL
        }
        get <- function() x
        setinv <- function(sol) invmatrix <<- sol
        getinv <- function() invmatrix
        list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("Getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        inv
}
