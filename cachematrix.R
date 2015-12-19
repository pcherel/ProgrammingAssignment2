## Two functions in this script file are provided to 
## cache the computed inverse of a given matrix.

## This is useful when one has to make several calls to use the inverse
## of the same matrix during any algorithm.

## Usage: 
## 1/ makeCacheMatrix() should be used first to create a cached version
## of the matrix to be cached. 
## 2/ cacheSolve() will return repeateadly 
## the inverse  of this cached matrix, but will compute it only once, 
## store it and reuse it in later calls

## Warning : 
## no checks are performed regarding matrix size, dimensions, 
## singularity of a given matrix and if it could be given an inverse or not.

## Information : 
## solve() is used to compute matrix inverse at first call of cacheSolve()

## ---------------------------------------------------------------------

## makeCacheMatrix function will return a list object, list of functions : 
## - to set and get inital data : set(), get()
## - to set and get a cached inverse value : setInverse(), getInverse()
## all list functions are based on the argument supplied matrix x.
makeCacheMatrix <- function ( x = matrix()){
        
        # Local variable to cache inverted matrix
        # ..initialized as NULL (when inverse was never computed)
        invMatrix <<- NULL ;
        
        # defines the set function to reset data value and cached inverse value 
        # ..take new data value as an argument
        set <- function (newMatrix = matrix()){
                x <<- newMatrix ;
                invMatrix <<- NULL ;
        }  
        
        # defines a get function to read matrix data from 
        get <- function() x ;
        
        # defines a get function to return cached inverse matrix, 
        # as locally stored 
        getInverse <- function() invMatrix ;
        
        # defines a setInverse function to set the local cache variable 
        # to computed inverse value. Takes one argument, inverted matrix value
        setInverse <- function ( invm = matrix ()) invMatrix <<- invm ;
        
        # creates and returns a list object encapsulating data access functions
        flist <- list( get = get, set = set, getInverse = getInverse, 
                       setInverse = setInverse )
        return(flist)
        
}


## -----------------------------------------------------------------------------------------

## cacheSolve function will return the inverted matrix  
## - from the "cached" version of matrix built with makeCacheMatrix
## - cached matrix is passed as first argument to cacheSolve()
## - return value is the inverse of cached matrix
## - computed once only and stored in cachedMatrix object
cacheSolve <- function ( x, ...){
        
        # x is a cached Matrix object created with previous
        # makeCachedMatrix() function. 
        
        # Attempts first to retrieve a cached value of matrix inverse
        invm <- x$getInverse() ;
        
        # Checks if cached value is null (first call)
        if ( ! is.null(invm)){
                
                # Message  
                message("getting cached matrix");
                # returns cached inverse value
                return(invm)
                # cacheSolve function execution stops here 
        }
        
        # if function didn't return here, cached inverse is null
        # and should be computed.
        
        # First, get matrix data
        m <- x$get()
        # inverse matrix data using solve()
        invm <- solve(m);
        # save inverse value in cache for future calls
        x$setInverse(invm) ;
        
        # returns newly computed inverse value
        return(invm);
}

