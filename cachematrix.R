## Below functions will employ a data caching functionality in order to increase the 
## performance of calculations on large input vectors. Instead of recalculating for 
## each call, it is verified whether the function was executed before and whether the 
## object can be retrieved from the cache; this is made possible by storing the object in
## a variable.

## This function, "makeCacheMatrix", will be used as an input for the function
## "cacheSolve". The class of the input is a matrix. It will set the value of the matrix;
## it will get the value of the matrix; it will set the value of an inverse matrix by the
## use of the build-in solve() function; and finally, it will get the value of the 
## inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix) 
}


## This function, "cacheSolve", will first verify whether the inverse matrix was already
## calculated, and thus stored "in cache". If the matrix is found, no further calculation
## is made and the output is returned to the console. The output data is of the matrix
## class. In the event that the data was not present in the cache (i.e. the function is 
## run for the first time), it will calculate the inverse of the input matrix by the use
## of the build-in solve() function, making use of the get function in order to obtain
## the input matrix. Afterwards, the output is assigned to the setmatrix function such
## that it can be recalled in later instances (if applicable).

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}
