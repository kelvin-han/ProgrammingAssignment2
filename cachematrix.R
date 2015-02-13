## makeCacheMatrix allows a function to be assigned to a variable
## e.g. inv <- makeCacheMatrix()
## use set to store the desired matrix
## e.g. inv$set(matrix(...))
## use get to return the desired matrix
## use setInverse to store the inverse of the matrix
## use getInverse to return the inverse of the matrix
## run cacheSolve to solve for inverse matrix
## e.g. cacheSolve(inv)

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialise m to NULL
        inv <- NULL
        
        ## Define set, get, setinverse and getinverse functions
        
        ## Sets the value of non-inverse matrix and 
        ## sets inverse of matrix to NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        ## Returns the non-inverse matrix
        get <- function () x
        
        ## Set the inverse of matrix
        setinverse <- function(solve) inv <<- solve
        
        ## Returns the inverse of matrix
        getinverse <- function() inv
        
        ## list set, get, setinverse and getinverse
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)    
}

## cacheSolve will retrieve the result if previously cached
## If the result is not available, then the inverse of the matrix
## is calculated and returned

cacheSolve <- function(x=matrix(), ...) {
        
        ## get inverse of matrix and assign to inv
        inv <- x$getinverse()
        
        ## if inverse of matrix is not null, return inv
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        ## get non-inverse matrix
        data <- x$get()
        
        ## solve for inverse matrix
        inv <- solve(data, ...)
        
        ## stores inverse matrix in cache
        x$setinverse(inv)
        
        ## Return a matrix that is the inverse of x
        inv
        
}
