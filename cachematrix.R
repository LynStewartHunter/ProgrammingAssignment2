## LSH 3/12/17 cachematric.R JH R class Week3
## creates a matrix of funtions to store the mean
## in the global memory
## creates matrix and sets list of functions

## this is the makeVector Function with inverse replacing mean
## replaced all m's with an Vinv
## creates some functions and a matrix from passed in valuesii
makeCacheMatrix <- function(x = matrix()) {
        Vinv <- NULL  ## initialize the inverse to null
        set <- function(y) {
                x <<- y
                Vinv <<- NULL  ## reset any existing values
        }
        get <- function() x 
        setInverse <- function(Inverse) Vinv <<- Inverse
        getInverse <- function() Vinv
        ## create the function list
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This should return the inverse of the matrix, but there is an issue
## with the matrix not being square, an issue for which I cannot find a solution
## test cacheSolve(makeCacheMatrix(matrix(1:9, nrow = 3, ncol = 3)))

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x' [which comes from function above]
     inv <- x$getInverse()
     ## inv <- x$get() ## see if value is cached
     if (is.null(inv)) {   ##  not stored
       tempmatrix <- x$get()
       inv <- solve(tempmatrix, ...)
       x$setInverse(inv)
       return(inv)
       
    } else 
       return(inv)  ## already cached
   }
   }