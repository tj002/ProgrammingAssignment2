## Calcuates inverse of Matrix. 

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) 
            {
                x <<- y
                m <<- NULL
            }
    
    get <- function() x
    setmatrix <- function(mat) m <<- mat
    getmatrix <- function() m
    list( set = set,get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
## will retrieve the inverse from the cache.
cacheSolve <- function (x,...) {
# check square matrix and det(matrix) <> 0
    cachemat <- x$getmatrix()
    if (!is.null(cachemat)) 
    { 
        message("Getting Cache Data") 
        return(cachemat)  
    }
    dat <- x$get()
    mat <- solve(dat, ...)    
    x$setmatrix(mat)
    return(mat)
}


