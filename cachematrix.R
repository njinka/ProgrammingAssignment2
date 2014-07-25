
## Following function will creates a list containing a function to 
## set and get matrix, 
## set and get inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(inverse) inver <<- inverse
        getInverseMatrix <- function() inver
        list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
}


## Following function will first check if the inverse of a matrix has already 
## been calculated and returns the same if it did else calculates 
## the inverse of a matrix and returns it

cacheSolve <- function(x, ...) {
        inver <- x$getInverseMatrix()
        if(!is.null(inver)) {
                message("getting cached data")
                return inver
        }
        
        data <- x$get()
        inver <- solve(data, ...)
        x$setInverseMatrix(inver)
        inver
}
