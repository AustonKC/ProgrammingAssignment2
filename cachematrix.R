## Caching the Inverse of a Matrix
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
        c <- NULL
        set <- function(y) {
                x <<- y
                c <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) c <<- inverse
        getinverse <- function() c
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
        c <- x$getinverse()
        if (!is.null(c)) {
                message("getting cached data")
                return(c)
        }
        matrix <- x$get()
        c <- solve(matrix, ...)
        x$setinverse(c)
        c
}
