## A set function to create a matrix object which save, return and calculated the invers if it is already set or calculated.

## This function is to create a matrix with saves automaticaly its invers matrix

makeCacheMatrix <- function(x = matrix()) {
    invers <- NULL
    set <- function(y) {
        x <<- y
        invers <<- NULL
    }
    get <- function() x
    setinvers <- function(inv) invers <<- inv
    getinvers <- function() invers
    list(set = set, get = get,
         setinvers = setinvers,
         getinvers = getinvers)
}


## This function return the invers of a matrix, if it is possible. Otherwise there is an massage.
## If the invers is already calculated its returns the save invers. 
## If not, the function saves the invers for later...g

cacheSolve <- function(x, ...) {
    invers <- x$getinvers()
    if(!is.null(invers)) {
        message("getting cached data")
        return(invers)
    }
    data <- x$get()
    if(det(data)!=0){
        invers <- solve(data, ...)
        x$setinvers(invers)
        invers
    }
    else
        message("Matrix in not invertable")
}
