# The functions solves a given matrix and prints the inverse of the matrix. The functions will cache the result
# of the inverse operation and will not compute the inverse unless a new matrix is given as input.

# makeCacheMatrix function will take a matrix as input and generate setter and getter functions. 
# the function returns a list object with the setter and getter functions as objects.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() { return(x) }
        setinverse <- function(inverse){ return(inv <<- inverse) }
        getinverse <- function(){ return(inv) }
        return(list(set = set, get = get,
                    setinverse = setinverse,
                    getinverse = getinverse))

}


# cacheSolve function takes the list object from the makeCacheMatrix function and check if the inverse
# for that matrix is already computed, if not it computes it and returns the result. In case the result is
# already computed and in memory, it will print the result.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        return(inv)
}
