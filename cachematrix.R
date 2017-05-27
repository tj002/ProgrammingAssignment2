## Calcuates inverse of Matrix. Assumption: Matrix given as input is always invertible

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { 
    m <- NULL                           # initialization of vector m where inverse of matrix would be cached 
    set <- function(y)                  # function will intialize the vectors with given values
            {
                x <<- y                 # initialize x to store matrix whose inverse is to be caluclated 
                m <<- NULL              # initialize m to null 
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
    cachemat <- x$getmatrix()           # get the cached matrix from makeCacheMatrix function
    if (!is.null(cachemat))             # check if cachemat has previously stored value
    { 
        message("Getting Cache Data")   # message to display
        return(cachemat)                # return cachemat value
    }
    dat <- x$get()                      # if inverse value is not cached then get the input matrix  
    mat <- solve(dat, ...)              # calculates inverse of input matrix
    x$setmatrix(mat)                    # caching the inverse value
    return(mat)                         # returning of caluclated value
}


