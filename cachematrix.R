## makeCacheMatrix creates a matrix object that can cache the inverse of the matrix.
## 
makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set <- function(y){
                y <<- x
                inv_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv_matrix <<- inverse
        getinverse <- function() inv_matrix
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
## cacheSolve reads in a matrix, checks if it has been calculated or changed and calculates
## the inverse of the matrix if needed.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_matrix <- x$getinverse()
        ## If nothing has changed and the cache is calculated, get the cashed data.
        if (!is.null(inv_matrix)) {
                message("getting cashed data...")
                return(inv_matrix)
        }
        ## If matrix has changed or the inverse have not been calculated, calculate the inverse of the matrix.
        data <- x$get()
        inv_matrix <- solve(data, ...)
        x$setinverse(inv_matrix)
        return(inv_matrix)
}