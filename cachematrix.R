## The makeCacheMatrix() function takes a matrix and stores both its content and the content of its inverse with functions set() and SetInverse(), which can then be retrieved using the functions get() and GetInverse().

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv  <<- NULL
        }
        get <- function() {x} 
        SetInverse <- function(inverse) inv <<- inverse
        GetInverse <- function() {inv}
        list(set = set, get = get,
             SetInverse = SetInverse,
             GetInverse = GetInverse)
}


##The if loop checks if the inverse has already been calculated and, if so, it can get the inverse from the cache and avoid recalculating it. Otherwise, it will recalculate, it will retrieve the matrix using get() funtcion defined within the makeCacheMatrix() and calculate its inversion using the solve() function.It will then cache the result using SetInverse() and print the inverse.
cacheSolve <- function(x, ...) {
        inv <- x$GetInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$SetInverse(inv)
        inv
}
