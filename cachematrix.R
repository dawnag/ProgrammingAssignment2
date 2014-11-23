####################################################################
##
##  Programming Assignment #2: Lexical Scoping: 
##                             Caching the Inverse of a Matrix
##------------------------------------------------------------------
##  Matrix inversion can be a time-consuming computation and
##  caching results may offer some time-saving benefits. These
##  two functions, makeCacheMatrix and cacheSolve, find the 
##  inverse of a matrix x. If the inverse has already been
##  calulated and cached, then there is no need to recompute
##  and time is saved. If it is not already cached, then it
##  is computed and saved in cache.
## 
####################################################################
##
####################################################################
##
##  This R function, makeCacheMatrix, is able to cache 
##  potentially time-consuming inverse matrix computations.
##  It creates a special "matrix", which is really a list
##  containing a function to set and get the value of the
##  matrix, and set and get the value of the inverse. If
##  the inverse has already been calulated (and the matrix
##  has not changed), then the cacheSolve function will 
##  retrieve the inverse from the cache.
##
####################################################################
makeCacheMatrix <- function(x = matrix()) {
        ## Initialize m
        m <- NULL    
        ## Set the value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## Get the value of the matrix
        get <- function() x
        ## Set the value of the inverse
        ## Cache the value of the inverse so that it can be looked
        ## up in the cache rather than be recomputed.
        setInvertMat <- function(InvertMat) m <<- InvertMat
        ## Get the Value of the inverse
        getInvertMat<- function() m
        list(set = set, get = get, 
             setInvertMat = setInvertMat,
             getInvertMat = getInvertMat)
}

####################################################################
##
##  This function, cacheSolve, returns a matrix that is the
##  inverse of 'x'. This secondary function actually computes
##  the inverse of the special "matrix" provided by its
##  partner function, makeCacheMatrix. If the inverse has
##  already been calculated (and the matrix has not changed),
##  then cacheSolve retrieves the inverse from the cache. If
##  not previously calculated, it is calculated, and then
##  saved to cache.
##
##  Note: For the purposes of this assignment, it is assumed that
##        the supplied matrix is always invertible.
##
####################################################################
cacheSolve <- function(x, ...) {
        
        ## See what's in the cache.
        m <- x$getInvertMat())
        
        ## If not null, then the inverse has already been calculated.
        ## Retrieve the cached data.
        if(!is.null(m)) {
                message("Matrix inverse already calculated.")
                message("Retrieving cached data...")
        }
        else {  ## Inverse not previously computed and saved in cache, 
                ## and must be calculated
                message("Calculating matrix inverse...")
                data <- x$get()
                ## Compute the inverse of the matrix. 
                m <- solve(data, ...)
                ## Set the value of the inverse in the cache using
                ## the setInvertMat function.
                x$setInvertMat(m)
        }
        message("...done.")
        ## Return the inverse
        return(m)
}
