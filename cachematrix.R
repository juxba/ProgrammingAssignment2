## cachematrix is a cache mechanism for matrices and theirs inverses
## 
## makeCacheMatrix delivers an R object consisting of a list of functions for 
## managing the given matrix and its calculated inverse,
## both saved in the parent environment.
## 
## Argument x is an invertible matrix.
## Usage:
## > x = matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), 3) ## An invertible matrix 
## > r <- makeCacheMatrix(x)
## > s <- cacheSolve(r)
## > s <- cacheSolve(r) ## this time solve() is not called
## getting cached data
## > r$set(matrix(1:9, 3))  ## reusable variable name
## set(x): x is not invertible
## > r$get()
## >

makeCacheMatrix <- function(x = matrix()) {
    
    checkmatrix <- function(y, funcname) {
        if(!is.matrix(y)) {
            message(paste(funcname, "(x): x is not a matrix", sep = ""))
            return(FALSE)
        }
        if(!(dim(y)[1] == dim(y)[2])) {
            message(paste(funcname, "(x): x is not a quadratic matrix", sep = ""))
            return(FALSE)
        }
        if(!is.numeric(y)) {
            message(paste(funcname, "(x): x must be numeric", sep = ""))
            return(FALSE)
        }
        if(det(y) == 0) {
            message(paste(funcname, "(x): x is not invertible", sep = ""))
            return(FALSE)
        }
        return(TRUE)
    }
    if(!checkmatrix(x, "makeCacheMatrix")) return()
    
    m <- NULL
    set <- function(y) {
        if (checkmatrix(y, "set")) {
            x <<- y
            m <<- NULL
        }
    }
    get <- function() x
    setinvers <- function(invers) m <<- invers
    getinvers <- function() m
    list(set = set, get = get,
         setinvers = setinvers,
         getinvers = getinvers)
}


## cacheSolve calls solve only once. Succesive calls return the cached inverse.
## Argument x is a list obtained from makeCacheMatrix(x)

cacheSolve <- function(x, ...) {
    
    if (!((length(x) == 4) & ("set" %in% names(x) & ("get" %in%  names(x))
                              & ("setinvers" %in%  names(x)) & ("getinvers" %in%  names(x))))) {
        message("cacheSolve(x): x must be a list obtained from makeCacheMatrix(x)")
        return(x)
    }
    
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinvers()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinvers(m)
    m
}
