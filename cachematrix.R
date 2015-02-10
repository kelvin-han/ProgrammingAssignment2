## makeCacheMatrix allows a function to be stored in a variable
## m <- makeCacheMatrix()
## use set to store the desired matrix
## m$set(matrix(...))
## run cacheSolve to solve for inverse matrix
## cacheSolve(m)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## define set, get, setinverse and getinverse
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        
        ## list set, get, setinverse and getinverse
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)    
}


## Write a short comment describing this function

cacheSolve <- function(x=matrix(), ...) {
        
        ## get inverse of matrix and assign to m
        m <- x$getinverse()
        
        ## if inverse of matrix is not null, return m
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        
        ## get non-inverse matrix
        data <- x$get()
        
        ## solve for inverse matrix
        m <- solve(data, ...)
        
        ## stores inverse matrix in cache
        x$setinverse(m)
        
        ## Return a matrix that is the inverse of 'x'
        m
        
}
