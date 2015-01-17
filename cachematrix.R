## author: Marc Sol
## date:   January 17, 2015
## Coursera course: R Programming, 
##                  Programming Assignment 2: Lexical Scoping

## This code implement two methods to optimize working with inversible matrices and their inverse.
## makeCacheMatrix creates a"cacheMatrix" data structure that encapsulates a matrix and stores
## its inverse once it has been calculated.
## cacheSolve returns the inverse of the matrix encapsulated in a "cacheMatrix"

## Creates a data structure (list) with 4 functions to set/get an inversible matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## in the environment of this function:
    ## x is the inversibe matrix
    ## inverse is the inverse matrix of x (or NULL if not yet calculated)
    
    inverse <- NULL ## will hold the cached inverse after it has been calculated
    
    set <- function(y) {
        x <<- y  ## sets value x in the parent environment (i.e. the env of function makeCacheMatrix)
        inverse <<- NULL ## clear the cache (when matrix x is changed, an existing inverse is no longer applicable)
    }
    
    get <- function() x
    
    setinverse <- function(inv) inverse <<- inv
    
    getinverse <- function() inverse
    
    ## create and return the cacheMatrix data structure (list with 4 functions)
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## If x is a cacheMatrix ethat encapsulates reversible matrix M, then this function returns the inverse of M  
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    ## First check if the inverse has been calculated already.
    ## If so, then it has been stored in x and can be retrieved throught the getinverse function
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        ## inverse has already been calculated. Return it.
        return(inverse)
    }
    ## inverse has not yet been calculated. Calculate it now.
    data <- x$get()
    inv <- solve(data) ## note that we may assume (as per assigment) that the inverse exists
    ## ! I deliberately did not carry the ... arguments to the solve function !
    ## First, providing a second argument (see ?solve documentation) would not produce the inverse
    ## Second, we would need to store an inverse matrix for each different configuration of ... values.
    ## Note that this is actually a bug in the example code for cachemean!
    
    ## Store the inverse for future re-use.
    x$setinverse(inv)
    ## return the calculated inverse
    inv
}

## ------------------------------------------------------------------------------
## Here ends the code for the assignment

## ------------------------------------------------------------------------------
## This function tests the above functions
## Since the test generates a random matrix, it may accidentally not be reversible.
test <- function() {
    ## create a random 3x3 matrix
    m <- matrix( rnorm(9), 3, 3)
    ## create the cache matrix and use it
    cm <- makeCacheMatrix(m)
    inv <- cacheSolve(cm)
    ## Multiply the matrix and its inverse.
    ## This should return the identity matrix (within numeric precision)
    print( inv %*% m)
}

## ------------------------------------------------------------------------------
## Below is an alternative implementation of cache matrix, which I believe is better:
## * all logic is encapsulated in one main function
## * it is not possible to obtain a NULL inverse (which happens in above implementation if user forgot to call cacheSolve)
## Hence this alternative is easier to use. Next to that it consists of less lines of code.
## Method testAlternative shows how to use this alternative.
## Arguably, we could even remove the setMatrix function. It should not be a problem to call makeAlternativeCacheMatrix
## multiple times, and hence create multiple "cache matrix lists". But I kept this in to not reduce the functionality of the
## intended code.
testAlternative <- function() {
    ## create a random 3x3 matrix
    m <- matrix( rnorm(9), 3, 3)
    ## create the cache matrix and use it
    cm <- makeAlternativeCacheMatrix(m)
    inv <- cm$getInverse()
    ## Multiply the matrix and its inverse.
    ## This should return the identity matrix (within numeric precision)
    print( inv %*% m)
}

## Creates a data structure (list) with 3 functions to efficiently get and store a matrix and its inverse
makeAlternativeCacheMatrix <- function(mat = matrix()) {
    ## in the environment of this function:
    ## mat (input argument) is the inversibe matrix
    ## inverse (local variable) is the inverse of matrix mat (or NULL if not yet calculated)
    
    inverse <- NULL ## will hold the cached inverse after it has been calculated
    
    setMatrix <- function(m) {
        mat <<- m  ## sets value mat in the parent environment
        ## When matrix mat is changed, an existing inverse is no longer applicable.
        ## Clear the stored inverse
        inverse <<- NULL
        ## Do not yet calculate the inverse here. That would waste time if it is never needed.
        ## The inverse will be calculated lazily inside function getInverse
    }
    
    getMatrix <- function() {
        mat
    }
    
    getInverse <- function() {
        if(is.null(inverse)) {
            ## inverse has not yet been calculated.
            ## Calculate it now, and store it for future re-use.
            inverse <<- solve(mat)
        }
        ## return the inverse
        inverse
    }
    
    ## create and return the cacheMatrix data structure (list with 3 functions)
    list(setMatrix = setMatrix, getMatrix = getMatrix, getInverse = getInverse)
}



