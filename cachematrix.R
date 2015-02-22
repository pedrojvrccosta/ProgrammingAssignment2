## Put comments here that give an overall description of what your
## functions do

## This function generates a list whose components are four functions/ methods that will be
## associated to a matrix that it receives as parameter. The functions implement the operations for 
## defining and fetching the matrix value, and defining and fetching its inverse

makeCacheMatrix <- function(x = matrix()) {
        
        x_inverse <- NULL
        
        set <- function( p_matrix ) {
                x <<- p_matrix
                x_inverse <- NULL       ##when matrix is defined its inverse is not yet available 
        }
        
        get <- function() x
        
        setInverse <- function( p_inverse ) x_inverse <<- p_inverse
        
        getInverse <- function() x_inverse
        
        list( set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse )
        
}



## The function returns the inverse of a 'cacheable' matrix passed to it as parameter 
## If the inverse of that matrix has already been calculated, then it is stored (cached)
## and so is fetched, without any further effort, and returned
## Otherwise, the inverse not yet calculated, is calculated, stored, and returned

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        ## Matrix is assumed to be invertible
        
        x_inverse <- x$getInverse()
        
        if( ! is.null(x_inverse) ) {
                return( x_inverse )
        }
        
        a_matrix <- x$get
        
        x_inverse <- solve( a_matrix )
        
        x$setInverse( x_inverse )
        
        x_inverse
}

