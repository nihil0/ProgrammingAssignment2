# Taking the inverse of a matrix is typically a time-consuming task. If the 
# contents of the matrix are not changing, we cache the value of the inverse 
# so that when we need it again, it can be looked up in the cache rather than 
# recomputed.


## Creates a data structure with functions to cache and fetch the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(inv) m <<- inv
    getInv <- function() m
    
    list(set=set,get=get,setInv=setInv,getInv=getInv)

}


# Computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if (!is.null(m)){
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setInv(m)
        m
}
