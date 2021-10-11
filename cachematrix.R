## Matrix inversion commonly consists of many computation and  some of its benefit 
## is to cache the inverse of a matrix that is comparative to computing repeatedly.

## Pair of functions that cache the inverse of a matrix
## It's usage is to pass the result of a makeCacheMatrix call to cacheSolve 
## This method is commonly used to create a matrix that can cache it's inverse input x
## as a matrix is the main argument

## The 2 functions that will be used are makeCacheMatrix and cacheSolve.
## makeCacheMatrix: The function creates a special "matrix" object 
## which could cache its inverse.
## cacheSolve: The function computes the inverse of the mentioned special "matrix"

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        ## cached inv of matrix
        inverse <- NULL
        
        ## Find number of rows and columns for x
        ncol <<- ncol(x)
        nrow <<- nrow(x)
        
        ## If it is not square matrix
        if (nrow != ncol) {
                stop("It is not a square matrix")
        }
        
        ## Define set function to set the value of matrix
        ## Sets inverse of x NULL as a matrix
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        ## get function gets the value of the matrix
        get <- function() return(x);
        
        ## setinverse function sets the value of the inverse
        setinverse <- function(inv) inv <<- inverse
        
        ## getinverse function sets the inverse of the matrix
        getinverse <- function() inv return(inverse)
        
        ## Return matrix
        return(list(set = set, get = get, 
             setinv = setinverse, 
             getinv= getinverse))
        }


## Compute and cache the inv of matrix
## This will compute the inverse of the 'x' returned above
## and retrieve the inverse from the code above.

## Returns:
## The inverse of the matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inv of 'x'
        
        ## Gets the inv of x
        inv <- x$getinverse()
        
        ## This is to check if the inverse of matrix is not NULL
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
}

data <- x$get()
inv <- solve (data,...)
x$setinverse(inv)
        return(inv)
}
